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
  ghcDep,
  LibPkgType(..),
  missingLibraries,
  missingOtherPkgs,
  missingPackages,
  notInstalled,
  notSrcOrInst,
  packageDependencies,
  pkgInstallMissing,
  pkgInstallMissing',
  pkgSuffix,
  subPackages,
  testsuiteDependencies'
  ) where

import PackageUtils (PackageData(..), prepare, removeLibSuffix,
                     repoquery, rpmInstall, stripPkgDevel)
import Types

import SimpleCabal (buildDependencies, mkPackageName, exeDepName,
                    PackageName, pkgcfgDepName, pkgName, prettyShow,
                    setupDependencies, testsuiteDependencies)
import SimpleCmd (cmd, cmdBool, warning, (+-+))
import SimpleCmd.Rpm (rpmspec)

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (filterM, when, unless)

import Data.List (delete, isPrefixOf, isSuffixOf, nub, (\\))
import Data.Maybe (catMaybes, fromJust, isNothing)

import Distribution.PackageDescription (PackageDescription (..),
                                        allBuildInfo,
                                        BuildInfo (..),
                                        )
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((<.>), (</>))

excludedPkgs :: PackageName -> Bool
excludedPkgs =
  flip notElem $ map mkPackageName ["ghc-prim", "integer-gmp"]

data LibPkgType = LibBase | LibDevel | LibProf | LibDoc | LibStatic

instance Show LibPkgType where
  show LibBase = ""
  show LibDevel = "devel"
  show LibProf = "prof"
  show LibDoc = "doc"
  show LibStatic = "static"

pkgSuffix :: LibPkgType -> String
pkgSuffix lpt =
  let rep = show lpt in
  if null rep then "" else '-' : rep

ghcDep :: LibPkgType -> PackageName -> String
ghcDep lt p = "ghc-" ++ prettyShow p ++ pkgSuffix lt

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
    in (deps, setup \\ (mkPackageName "Cabal" : deps), delete (prettyShow self) tools ++ cpp, clibs \\ ["m", stdcpp], pkgcfgs)

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

missingPackages :: PackageDescription -> IO [String]
missingPackages pkgDesc = do
  (deps, setup, tools, clibs, pkgcfgs) <- packageDependencies pkgDesc
  pcpkgs <- mapM derefPkg pkgcfgs
  filterM notSrcOrInst $ nub $ map (ghcDep LibDevel) (deps ++ setup) ++ ["ghc-Cabal-devel", "ghc-rpm-macros"] ++ tools ++ clibs ++ pcpkgs

missingLibraries :: PackageDescription -> IO [PackageName]
missingLibraries pkgDesc = do
  (deps, setup, _, _, _) <- packageDependencies pkgDesc
  bdeps <- filterM (notSrcOrInst . ghcDep LibProf) deps
  sdeps <- filterM (notSrcOrInst . ghcDep LibDevel) $ (mkPackageName "Cabal" : setup) \\ deps
  return $ bdeps ++ sdeps

missingOtherPkgs :: PackageDescription -> IO [String]
missingOtherPkgs pkgDesc = do
  (_, _, tools, clibs, pkgcfgs) <- packageDependencies pkgDesc
  pcpkgs <- mapM derefPkg pkgcfgs
  filterM notSrcOrInst $ nub $ ["ghc-rpm-macros"] ++ tools ++ clibs ++ pcpkgs

notSrcOrInst :: String -> IO Bool
notSrcOrInst pkg = do
  src <- doesDirectoryExist (".." </> removeLibSuffix pkg)
  if src then return False
    else notInstalled pkg

notInstalled :: String -> IO Bool
notInstalled dep =
  not <$> cmdBool "rpm" ["-q", "--quiet", "--whatprovides", shellQuote dep]
  where
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

pkgInstallMissing :: Flags -> Stream -> Maybe Package -> IO [String]
pkgInstallMissing flags stream mpkg = do
  pkgdata <- prepare flags stream mpkg True
  pkgInstallMissing' pkgdata

pkgInstallMissing' :: PackageData -> IO [String]
pkgInstallMissing' pkgdata = do
  let pkgDesc = packageDesc pkgdata
      mspec = specFilename pkgdata
  missing <- missingLibraries pkgDesc
  if null missing then return []
    else do
    subpkgs <- subPackages mspec pkgDesc
    let pkgs = map (ghcDep LibProf) $ missing \\ subpkgs
    pkgconfdir <- fromJust . lookup "Global Package DB" . read <$> cmd "ghc" ["--info"]
    putStrLn $ "Running repoquery" +-+ unwords pkgs
    repopkgs <- filter (/= "") <$> mapM (repoqueryPackageConf pkgconfdir) pkgs
    let missing' = pkgs \\ repopkgs
    unless (null missing') $ do
      putStrLn "Unavailable dependencies:"
      mapM_ putStrLn missing'
    unless (null repopkgs) $ do
      putStrLn "Uninstalled dependencies:"
      mapM_ putStrLn repopkgs
      -- fedora <- rpmEval "%fedora"
      -- let nogpgcheck = ["--nogpgcheck" | fedora `elem` []]
      rpmInstall True $ map showPkg repopkgs
    return missing'
        where
          showPkg :: String -> String
          showPkg p = if '(' `elem` p then show p else p

repoqueryPackageConf :: String -> String -> IO String
repoqueryPackageConf pkgconfd pkg =
  let key = if isGhcDevelPkg pkg
        then pkgconfd </> stripPkgDevel pkg ++ "-[0-9]*.conf"
        else pkg in
    repoquery ["--qf", "%{name}"] key

isGhcDevelPkg :: String -> Bool
isGhcDevelPkg p =
  "ghc-" `isPrefixOf` p && "-devel" `isSuffixOf` p
