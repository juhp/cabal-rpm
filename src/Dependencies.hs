{-# LANGUAGE CPP #-}

-- |
-- Module      :  Dependencies
-- Copyright   :  (C) 2012-2020  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Dependency info

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Dependencies (
  dependencies,
  hsDep,
  missingLibraries,
  missingOtherPkgs,
  missingPackages,
  notAvail,
  notInstalled,
  notSrcOrInst,
  packageDeps,
  PackageDependencies(..),
  packageDependencies,
  pkgInstallMissing,
  pkgInstallMissing',
  pkgSuffix,
  prettyShow,
  recurseMissing,
  showDep,
  subPackages,
  testsuiteDependencies'
  ) where

import PackageUtils (PackageData(..), prepare, repoquery, rpmInstall)
import SysCmd (rpmEval)
import Types

import SimpleCabal (allLibraries, buildDependencies, mkPackageName,
                    exeDepName, Library(..),
                    PackageDescription (package),
                    PackageIdentifier(..),
                    PackageName, pkgcfgDepName, pkgName,
                    setupDependencies, testsuiteDependencies,
#if !MIN_VERSION_Cabal(2,2,0)
                    showVersion,
#endif
#if MIN_VERSION_Cabal(2,0,0)
                    unPackageName
#endif
                   )
import SimpleCmd (cmd, cmdBool, removePrefix, removeSuffix, warning, (+-+))
import SimpleCmd.Rpm (rpmspec)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (filterM, when, unless)

import Data.List (delete, isSuffixOf, nub, (\\))
import Data.Maybe (catMaybes, fromJust, isJust, isNothing, mapMaybe)

#if !MIN_VERSION_Cabal(2,2,0)
import Distribution.License (License (..))
#endif
#if MIN_VERSION_Cabal(2,2,0)
import Distribution.Pretty (prettyShow)
#endif
import Distribution.Text (display)
import Distribution.PackageDescription (buildInfo, BuildInfo (..),
                                        executables, hasLibs, license,
                                        testBuildInfo, testSuites)
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Types.ExeDependency (ExeDependency(..))
#endif
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((<.>), (</>))

excludedPkgs :: PackageName -> Bool
excludedPkgs =
  flip notElem $ map mkPackageName ["ghc-prim", "integer-gmp", "rts", "system-cxx-std-lib"]

excludedTools :: String -> Bool
excludedTools n =
  n `notElem` ["ghc", "hsc2hs", "perl"]

pkgSuffix :: LibPkgType -> String
pkgSuffix lpt =
  if null rep then "" else '-' : rep
  where
    rep = show lpt

dependencies :: PackageDescription  -- ^pkg description
                -> ([PackageName], [PackageName], [String], [String], [String])
                -- ^depends, setup, tools, c-libs, pkgcfg
dependencies pkgDesc =
    let self = pkgName $ package pkgDesc
        deps = filter excludedPkgs $ buildDependencies pkgDesc
        setup = filter excludedPkgs $ setupDependencies pkgDesc
        buildinfo = [ bi | lib <- allLibraries pkgDesc
                         , let bi = libBuildInfo lib ] ++
                    [ bi | exe <- executables pkgDesc
                         , let bi = buildInfo exe ]
        tools =  nub $ map exeDepName (concatMap buildTools buildinfo)
        toolDeps' = concatMap buildToolDepends' buildinfo
        pkgcfgs = nub $ map pkgcfgDepName $ concatMap pkgconfigDepends buildinfo
        clibs = nub $ concatMap extraLibs buildinfo
        stdcpp = "stdc++"
        cpp = ["gcc-c++" | stdcpp `elem` clibs]
    in (deps, setup \\ (mkPackageName "Cabal" : deps), delete (display self) tools ++ cpp ++ toolDeps', clibs \\ ["m", stdcpp], pkgcfgs)

buildToolDepends' :: BuildInfo -> [String]
#if MIN_VERSION_Cabal(2,0,0)
buildToolDepends' buildinfo =
  map (unPackageName . \(ExeDependency pn _ _) -> pn) $
    buildToolDepends buildinfo
#else
buildToolDepends' _ =
      []
#endif

data QueryBackend = Rpm | Repoquery deriving Eq

resolveLib :: String -> IO (Maybe String)
resolveLib lib = do
  lib64 <- doesDirectoryExist "/usr/lib64"
  let libsuffix = if lib64 then "64" else ""
  let lib_path = "/usr/lib" ++ libsuffix ++ "/lib" ++ lib <.> "so"
  libInst <- doesFileExist lib_path
  mresult <-
    if libInst
    then rpmqueryFile Rpm lib_path
    else do
      putStrLn $ "Running repoquery for" +-+ lib_path
      rpmqueryFile Repoquery lib_path
  when (isNothing mresult) $
    warning $ "could not resolve dependency" +-+ lib
  return mresult

-- use repoquery or rpm -q to query which package provides file
rpmqueryFile :: QueryBackend -> FilePath -> IO (Maybe String)
rpmqueryFile backend file = do
  let args =  ["--qf=%{name}", "--whatprovides"]
  out <- if backend == Rpm
         then cmd "rpm" ("-q" : args ++ [file])
         else repoquery args file
  let pkgs = nub $ words out
  -- EL5 repoquery can return "No package provides <file>"
  case pkgs of
    [pkg] -> return $ Just pkg
    [] -> do
      warning $ "Could not resolve package that provides" +-+ file
      return Nothing
    _ -> do
      warning $ "More than one package seems to provide" +-+ file ++ ": " +-+ unwords pkgs
      return Nothing

data PackageDependencies = PackageDepends
                           { buildDeps :: [PackageName]
                           , setupDeps :: [PackageName]
                           , toolDeps ::[String]
                           , clibDeps :: [String]
                           , pkgcfgDeps :: [String]
                           }

packageDependencies :: PackageDescription  -- ^pkg description
                -> IO PackageDependencies
packageDependencies pkgDesc = do
    let (deps, setup, tools', clibs', pkgcfgs) = dependencies pkgDesc
        tools = filter excludedTools $ nub $ map mapTools tools'
    -- nothing provides libpthread.so
    clibsWithErrors <- mapM resolveLib $ delete "pthread" clibs'
    let clibs = catMaybes clibsWithErrors
    let showPkgCfg p = "pkgconfig(" ++ p ++ ")"
    return $ PackageDepends deps setup tools (nub clibs) (map showPkgCfg pkgcfgs)

mapTools :: String -> String
mapTools "cabal" = "cabal-install"
mapTools "gtk2hsC2hs" = "gtk2hs-buildtools"
mapTools "gtk2hsHookGenerator" = "gtk2hs-buildtools"
mapTools "gtk2hsTypeGen" = "gtk2hs-buildtools"
mapTools "hspec-discover" = "ghc-hspec-discover-devel"
mapTools tool = tool

testsuiteDependencies' :: PackageDescription -> ([PackageName],[String])
testsuiteDependencies' pkgDesc =
  let self = pkgName $ package pkgDesc in
  ((filter excludedPkgs . testsuiteDependencies) pkgDesc,
   map mapTools ((delete (display self) . nub) (testTools ++ testToolDeps)))
  where
    tests = map testBuildInfo $ testSuites pkgDesc
    testTools = map exeDepName $ concatMap buildTools tests
    testToolDeps = filter excludedTools $ concatMap buildToolDepends' tests

missingPackages :: PackageDescription -> IO [RpmPackage]
missingPackages pkgDesc = do
  pkgdeps <- packageDependencies pkgDesc
  pcpkgs <- mapM derefPkg $ pkgcfgDeps pkgdeps
  filterM notSrcOrInst $ nub $ map (RpmHsLib Devel) (buildDeps pkgdeps ++ setupDeps pkgdeps ++ [mkPackageName "Cabal"]) ++ map RpmOther (["ghc-rpm-macros"] ++ toolDeps pkgdeps ++ clibDeps pkgdeps ++ pcpkgs)

missingLibraries :: PackageDescription -> IO [PackageName]
missingLibraries pkgDesc = do
  pkgdeps <- packageDependencies pkgDesc
  -- just use Devel for deps because -devel presence implies -prof should exist
  bdeps <- filterM (notSrcOrInst . RpmHsLib Devel) $ buildDeps pkgdeps
  sdeps <- filterM (notSrcOrInst . RpmHsLib Devel) $ (mkPackageName "Cabal" : setupDeps pkgdeps) \\ buildDeps pkgdeps
  return $ bdeps ++ sdeps

uninstalledLibraries :: Bool -> PackageDescription -> IO [PackageName]
uninstalledLibraries hasLibPkg pkgDesc = do
  pkgdeps <- packageDependencies pkgDesc
  libType <-
    if hasLibPkg
    then withGhcProfLibType
    else return Devel
  bdeps <- filterM (notInstalled . RpmHsLib libType) $ buildDeps pkgdeps
  sdeps <- filterM (notInstalled . RpmHsLib Devel) $ (mkPackageName "Cabal" : setupDeps pkgdeps) \\ buildDeps pkgdeps
  return $ bdeps ++ sdeps

missingOtherPkgs :: PackageDescription -> IO [String]
missingOtherPkgs pkgDesc = do
  pkgdeps <- packageDependencies pkgDesc
  pcpkgs <- mapM derefPkg $ pkgcfgDeps pkgdeps
  filterM (notSrcOrInst . RpmOther) $ nub $ ["ghc-rpm-macros"] ++ toolDeps pkgdeps ++ clibDeps pkgdeps ++ pcpkgs

notSrcOrInst :: RpmPackage -> IO Bool
notSrcOrInst pkg = do
  src <- doesDirectoryExist (".." </> showRpm (baseLibPackage pkg))
  if src
    then return False
    else notInstalled pkg
  where
    baseLibPackage :: RpmPackage -> RpmPackage
    baseLibPackage (RpmHsLib _ n) = RpmHsLib Base n
    baseLibPackage p = p

notInstalled :: RpmPackage -> IO Bool
notInstalled dep =
  not <$> cmdBool "rpm" ["-q", "--quiet", "--whatprovides", quoteShow dep]
  where
    quoteShow :: RpmPackage -> String
    quoteShow (RpmOther cs) = shellQuote cs
    quoteShow p = showRpm p

    shellQuote :: String -> String
    shellQuote (c:cs) = (if c `elem` "()" then (['\\', c] ++) else (c:)) (shellQuote cs)
    shellQuote "" = ""

derefPkg :: String -> IO String
derefPkg req = do
  res <- singleLine <$> repoquery ["--qf", "%{name}", "--whatprovides"] req
  if null res
    then error $ req +-+ "provider not found by repoquery"
    else return res
  where
    singleLine :: String -> String
    singleLine "" = ""
    singleLine s = (head . lines) s

subPackages :: Maybe FilePath -> PackageDescription -> IO [PackageName]
subPackages mspec pkgDesc = do
  develSubpkgs <- map stripPkgDevel . filter ("-devel" `isSuffixOf`) <$> maybe (return []) (rpmspec [] (Just "%{name}")) mspec
  let self = pkgName $ package pkgDesc
  return $ delete self $ map mkPackageName develSubpkgs
  where
    stripPkgDevel :: String -> String
    stripPkgDevel = removeSuffix "-devel" . removePrefix "ghc-"

pkgInstallMissing :: Flags -> Maybe PackageVersionSpecifier -> IO [PackageName]
pkgInstallMissing flags mpvs = do
  pkgdata <- prepare flags Nothing mpvs
  pkgInstallMissing' pkgdata

pkgInstallMissing' :: PackageData -> IO [PackageName]
pkgInstallMissing' pkgdata = do
  let pkgDesc = packageDesc pkgdata
      mspec = specFilename pkgdata
  missing <- uninstalledLibraries (hasLibs pkgDesc) pkgDesc
  if null missing then return []
    else do
    subpkgs <- subPackages mspec pkgDesc
    let pkgs = missing \\ subpkgs
    pkgconfdir <- fromJust . lookup "Global Package DB" . read <$> cmd "ghc" ["--info"]
    putStrLn $ "Running repoquery for" +-+ unwords (map display pkgs)
    repopkgs <- catMaybes <$> mapM (repoqueryPackageConf pkgconfdir) pkgs
    let missing' = pkgs \\ repopkgs
    unless (null missing') $ do
      putStrLn "Unavailable dependencies:"
      mapM_ (putStrLn . display) missing'
    unless (null repopkgs) $ do
      putStrLn "Found dependencies:"
      mapM_ (putStrLn . display) repopkgs
      -- fedora <- rpmEval "%fedora"
      -- let nogpgcheck = ["--nogpgcheck" | fedora `elem` []]
      noMacros <- notInstalled (RpmOther "ghc-rpm-macros")
      libtype <- withGhcProfLibType
      rpmInstall True $ map (showRpm . RpmHsLib libtype) repopkgs ++ ["ghc-rpm-macros" | noMacros]
    return missing'
      where
        repoqueryPackageConf :: String -> PackageName -> IO (Maybe PackageName)
        repoqueryPackageConf pkgconfd pkg = do
              let key = pkgconfd </> display pkg ++ "-[0-9]*.conf"
              res <- repoquery ["--qf", "%{name}"] key
              return $ if null res then Nothing else  Just pkg

withGhcProfLibType :: IO LibPkgType
withGhcProfLibType = do
  with_ghc_prof <- rpmEval "{?with_ghc_prof}"
  return $ if isJust with_ghc_prof then Prof else Devel

showDep :: RpmPackage -> String
showDep (RpmHsLib _ n) = display n
showDep (RpmHsBin n) = display n
showDep p = showRpm p

hsDep :: RpmPackage -> Maybe PackageName
hsDep (RpmHsLib _ n) = Just n
hsDep _ = Nothing

recurseMissing :: Flags -> Maybe Stream -> [(PackageIdentifier,String)]
               -> [PackageName] -> IO [(PackageIdentifier,String)]
recurseMissing _ _ already [] = return already
recurseMissing flags mstream already (dep:deps) = do
  (miss,mpidLic) <- missingDepsPkg dep
  (more,accum) <-
    case mpidLic of
      Nothing -> do
        pids <- recurseMissing flags mstream already deps
        return (pids, already)
      Just (pid,lic) -> do
        putMissing miss
        let hmiss = mapMaybe hsDep miss
            accum1 = (pid,lic) : already
        deeper <- recurseMissing flags mstream accum1 hmiss
        let accum2 = nub $ accum1 ++ deeper
        pids <- recurseMissing flags mstream accum2 deps
        return (pids, accum2)
  return $ nub $ accum ++ more
  where
    missingDepsPkg :: PackageName
                   -> IO ([RpmPackage], Maybe (PackageIdentifier, String))
    missingDepsPkg pkg =
      if pkg `elem` map (pkgName . fst) already
      then return ([], Nothing)
      else do
        pkgdata <- prepare flags Nothing (streamPkgToPVS mstream (Just (unversionedPkgId pkg)))
        let pkgdesc = packageDesc pkgdata
        missdeps <- missingPackages pkgdesc >>= filterM notAvail
        let pid = package pkgdesc
            licensestr = prettyShow $ license pkgdesc
        return (missdeps, Just (pid, licensestr))

    putMissing :: [RpmPackage] -> IO ()
    putMissing [] = return ()
    putMissing depends = putStrLn $ "  " ++ "needs:" +-+ unwords (markAlready depends)
      where
        markAlready :: [RpmPackage] -> [String]
        markAlready [] = []
        markAlready (d:ds) =
          let (op, cl) = if alreadyMentioned d then ("(", ")") else ("", "") in
            (op ++ showDep d ++ cl) : markAlready ds

        alreadyMentioned :: RpmPackage -> Bool
        alreadyMentioned d = maybe False (`elem` map (pkgName . fst) already) (hsDep d)

notAvail :: RpmPackage -> IO Bool
notAvail pkg = null <$> repoquery [] (showRpm pkg)

packageDeps :: Flags -> PackageIdentifier -> IO [PackageName]
packageDeps flags pid = do
  pkgdata <- prepare flags Nothing (Just $ PVPackageId pid)
  let pkgDesc = packageDesc pkgdata
      (deps, setup, _, _, _) = dependencies pkgDesc
  return $ nub $ (deps ++ setup) \\ [pkgName pid]

-- FIXME convert strings to SPDX or drop?
#if !MIN_VERSION_Cabal(2,2,0)
prettyShow :: License -> String
prettyShow (GPL Nothing) = "GPL+"
prettyShow (GPL (Just ver)) = "GPLv" ++ showVersion ver ++ "+"
prettyShow (LGPL Nothing) = "LGPLv2+"
prettyShow (LGPL (Just ver)) = "LGPLv" ++ [head $ showVersion ver] ++ "+"
prettyShow BSD3 = "BSD"
prettyShow BSD4 = "BSD"
prettyShow MIT = "MIT"
prettyShow PublicDomain = "Public Domain"
prettyShow AllRightsReserved = "Proprietary"
prettyShow OtherLicense = "Unknown"
prettyShow (UnknownLicense l) = removePrefix "LicenseRef" l  -- FIXME
#if MIN_VERSION_Cabal(1,16,0)
prettyShow (Apache Nothing) = "ASL ?"
prettyShow (Apache (Just ver)) = "ASL" +-+ showVersion ver
#endif
#if MIN_VERSION_Cabal(1,18,0)
prettyShow (AGPL Nothing) = "AGPLv?"
prettyShow (AGPL (Just ver)) = "AGPLv" ++ showVersion ver
#endif
#if MIN_VERSION_Cabal(1,20,0)
prettyShow BSD2 = "BSD"
prettyShow (MPL ver) = "MPLv" ++ showVersion ver
#endif
#if MIN_VERSION_Cabal(1,22,0)
prettyShow ISC = "ISC"
prettyShow UnspecifiedLicense = "Unspecified license!"
#endif
#endif
