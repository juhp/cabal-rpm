{-# LANGUAGE CPP #-}

-- |
-- Module      :  Dependencies
-- Copyright   :  (C) 2012-2019  Jens Petersen
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
  packageDependencies,
  pkgInstallMissing,
  pkgInstallMissing',
  pkgSuffix,
  recurseMissing,
  showDep,
  subPackages,
  testsuiteDependencies'
  ) where

import PackageUtils (PackageData(..), prepare, repoquery, rpmInstall)
import Types

import SimpleCabal (buildDependencies, mkPackageName,
                    exeDepName,
                    PackageDescription (package),
                    PackageIdentifier,
                    PackageName, pkgcfgDepName, pkgName,
                    setupDependencies, testsuiteDependencies)
import SimpleCmd (cmd, cmdBool, removePrefix, removeSuffix, warning, (+-+))
import SimpleCmd.Rpm (rpmspec)

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (filterM, when, unless)

import Data.List (delete, isSuffixOf, nub, (\\))
import Data.Maybe (catMaybes, fromJust, isNothing, mapMaybe)

import Distribution.Text (display)
import Distribution.PackageDescription (allBuildInfo, BuildInfo (..),  hasLibs)

import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((<.>), (</>))

excludedPkgs :: PackageName -> Bool
excludedPkgs =
  flip notElem $ map mkPackageName ["ghc-prim", "integer-gmp"]

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
        buildinfo = allBuildInfo pkgDesc
        tools =  nub $ map exeDepName (concatMap buildTools buildinfo)
        pkgcfgs = nub $ map pkgcfgDepName $ concatMap pkgconfigDepends buildinfo
        clibs = nub $ concatMap extraLibs buildinfo
        stdcpp = "stdc++"
        cpp = ["gcc-c++" | stdcpp `elem` clibs]
    in (deps, setup \\ (mkPackageName "Cabal" : deps), delete (display self) tools ++ cpp, clibs \\ ["m", stdcpp], pkgcfgs)

data QueryBackend = Rpm | Repoquery deriving Eq

resolveLib :: String -> IO (Maybe String)
resolveLib lib = do
  lib64 <- doesDirectoryExist "/usr/lib64"
  let libsuffix = if lib64 then "64" else ""
  let lib_path = "/usr/lib" ++ libsuffix ++ "/lib" ++ lib <.> "so"
  libInst <- doesFileExist lib_path
  if libInst
    then rpmqueryFile Rpm lib_path
    else do
    putStrLn $ "Running repoquery for" +-+ lib_path
    rpmqueryFile Repoquery lib_path

-- use repoquery or rpm -q to query which package provides file
rpmqueryFile :: QueryBackend -> FilePath -> IO (Maybe String)
rpmqueryFile backend file = do
  let args =  ["-q", "--qf=%{name}", "-f"]
  out <- if backend == Rpm
         then cmd "rpm" (args ++ [file])
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

packageDependencies :: PackageDescription  -- ^pkg description
                -> IO ([PackageName], [PackageName], [String], [String], [String])
                -- ^depends, setup, tools, c-libs, pkgcfg
packageDependencies pkgDesc = do
    let (deps, setup, tools', clibs', pkgcfgs) = dependencies pkgDesc
        excludedTools n = n `notElem` ["ghc", "hsc2hs", "perl"]
        mapTools "cabal" = "cabal-install"
        mapTools "gtk2hsC2hs" = "gtk2hs-buildtools"
        mapTools "gtk2hsHookGenerator" = "gtk2hs-buildtools"
        mapTools "gtk2hsTypeGen" = "gtk2hs-buildtools"
        mapTools "hspec-discover" = "ghc-hspec-discover-devel"
        mapTools tool = tool
        tools = filter excludedTools $ nub $ map mapTools tools'
    clibsWithErrors <- mapM resolveLib clibs'

    when (any isNothing clibsWithErrors) $
      warning "could not resolve all clib dependencies"
    let clibs = catMaybes clibsWithErrors
    let showPkgCfg p = "pkgconfig(" ++ p ++ ")"
    return (deps, setup, tools, nub clibs, map showPkgCfg pkgcfgs)

testsuiteDependencies' :: PackageDescription -> [PackageName]
testsuiteDependencies' =
  filter excludedPkgs . testsuiteDependencies

missingPackages :: PackageDescription -> IO [RpmPackage]
missingPackages pkgDesc = do
  (deps, setup, tools, clibs, pkgcfgs) <- packageDependencies pkgDesc
  pcpkgs <- mapM derefPkg pkgcfgs
  filterM notSrcOrInst $ nub $ map (RpmHsLib Devel) (deps ++ setup ++ [mkPackageName "Cabal"]) ++ map RpmOther (["ghc-rpm-macros"] ++ tools ++ clibs ++ pcpkgs)

missingLibraries :: Bool -> PackageDescription -> IO [PackageName]
missingLibraries hasLibPkg pkgDesc = do
  (deps, setup, _, _, _) <- packageDependencies pkgDesc
  let libType = if hasLibPkg then Prof else Devel
  bdeps <- filterM (notSrcOrInst . RpmHsLib libType) deps
  sdeps <- filterM (notSrcOrInst . RpmHsLib Devel) $ (mkPackageName "Cabal" : setup) \\ deps
  return $ bdeps ++ sdeps

missingOtherPkgs :: PackageDescription -> IO [String]
missingOtherPkgs pkgDesc = do
  (_, _, tools, clibs, pkgcfgs) <- packageDependencies pkgDesc
  pcpkgs <- mapM derefPkg pkgcfgs
  filterM (notSrcOrInst . RpmOther) $ nub $ ["ghc-rpm-macros"] ++ tools ++ clibs ++ pcpkgs

notSrcOrInst :: RpmPackage -> IO Bool
notSrcOrInst pkg = do
  src <- doesDirectoryExist (".." </> showRpm (baseLibPackage pkg))
  if src then return False
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

pkgInstallMissing :: Flags -> Stream -> Maybe PackageIdentifier -> IO [PackageName]
pkgInstallMissing flags stream mpkgid = do
  pkgdata <- prepare flags stream mpkgid True
  pkgInstallMissing' pkgdata

pkgInstallMissing' :: PackageData -> IO [PackageName]
pkgInstallMissing' pkgdata = do
  let pkgDesc = packageDesc pkgdata
      mspec = specFilename pkgdata
  missing <- missingLibraries (hasLibs pkgDesc) pkgDesc
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
      rpmInstall True $ map (showRpm . RpmHsLib Prof) repopkgs
    return missing'
      where
        repoqueryPackageConf :: String -> PackageName -> IO (Maybe PackageName)
        repoqueryPackageConf pkgconfd pkg = do
              let key = pkgconfd </> display pkg ++ "-[0-9]*.conf"
              res <- repoquery ["--qf", "%{name}"] key
              return $ if null res then Nothing else  Just pkg

showDep :: RpmPackage -> String
showDep (RpmHsLib _ n) = display n
showDep (RpmHsBin n) = display n
showDep p = showRpm p

hsDep :: RpmPackage -> Maybe PackageName
hsDep (RpmHsLib _ n) = Just n
hsDep _ = Nothing

recurseMissing :: Flags -> Stream -> [PackageName] -> [PackageName] -> IO [PackageName]
recurseMissing _ _ already [] = return already
recurseMissing flags stream already (dep:deps) = do
  miss <- missingDepsPkg dep
  putMissing miss
  let hmiss = mapMaybe hsDep miss
  let accum = nub (dep : hmiss ++ already)
  -- deeper <- recurseMissing flags stream accum (miss \\ accum)
  -- let accum2 = nub $ accum ++ deeper
  more <- recurseMissing flags stream accum (deps \\ accum)
  return $ nub $ accum ++ more
  where
    missingDepsPkg :: PackageName -> IO [RpmPackage]
    missingDepsPkg pkg = do
      pkgdata <- prepare flags stream (Just (unversionedPkgId pkg)) False
      missingPackages (packageDesc pkgdata) >>= filterM notAvail

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
        alreadyMentioned d = maybe False (`elem` already) (hsDep d)

notAvail :: RpmPackage -> IO Bool
notAvail pkg = null <$> repoquery [] (showRpm pkg)

packageDeps :: Flags -> Stream -> PackageName -> IO [PackageName]
packageDeps flags stream pkg = do
  pkgdata <- prepare flags stream (Just (unversionedPkgId pkg)) False
  let pkgDesc = packageDesc pkgdata
      (deps, setup, _, _, _) = dependencies pkgDesc
  return $ nub $ (deps ++ setup) \\ [pkg]
