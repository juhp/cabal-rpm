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
  missingPackages,
  notInstalled,
  packageDependencies,
  pkgInstallMissing,
  showDep,
  subPackages,
  testsuiteDependencies
  ) where

import PackageUtils (PackageData(..), packageName, packageManager, repoquery)
import SimpleCmd (cmd, cmd_, cmdBool, (+-+))
import SimpleCmd.Rpm (rpmspec)
import SysCmd (optionalProgram, rpmEval, trySystem)

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (filterM, when, unless)

import Data.List (delete, isSuffixOf, nub, (\\))
import Data.Maybe (catMaybes, fromMaybe, isNothing)

import Distribution.Package  (Dependency (..),
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,22,0)
                              unPackageName,
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
                              unPkgconfigName
#endif
#else
                              PackageName (..)
#endif
                                       )

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
import Distribution.Types.LegacyExeDependency (LegacyExeDependency (..))
import Distribution.Types.PkgconfigDependency (PkgconfigDependency (..))
#endif

import Distribution.PackageDescription (PackageDescription (..),
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,4,0)
                                        allBuildDepends,
#endif
                                        allBuildInfo,
                                        BuildInfo (..),
                                        TestSuite (..),
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,24,0)
                                        setupDepends
#endif
                                        )
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((<.>))
import System.IO (hPutStrLn, stderr)
import System.Posix.User (getEffectiveUserID)

excludedPkgs :: String -> Bool
excludedPkgs = flip notElem ["Cabal", "base", "ghc-prim", "integer-gmp"]

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,4,0)
#else
allBuildDepends :: PackageDescription -> [Dependency]
allBuildDepends = buildDepends
#endif

-- returns list of deps
buildDependencies :: PackageDescription -> String -> [String]
buildDependencies pkgDesc self =
  let deps = nub $ map depName (allBuildDepends pkgDesc)
                   ++ setupDependencies pkgDesc
  in filter excludedPkgs (delete self deps)

setupDependencies :: PackageDescription  -- ^pkg description
                  -> [String]         -- ^depends
setupDependencies pkgDesc =
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,24,0)
  maybe [] (map depName . setupDepends) (setupBuildInfo pkgDesc)
#else
  []
#endif

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,22,0)
#else
unPackageName :: PackageName -> String
unPackageName (PackageName n) = n
#endif

depName :: Dependency -> String
depName (Dependency  n _) = unPackageName n

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
exeDepName :: LegacyExeDependency -> String
exeDepName (LegacyExeDependency n _) = n

pkgcfgDepName :: PkgconfigDependency -> String
pkgcfgDepName (PkgconfigDependency n _) = unPkgconfigName n
#else
exeDepName :: Dependency -> String
exeDepName = depName

pkgcfgDepName :: Dependency -> String
pkgcfgDepName = depName
#endif

showDep :: String -> String
showDep p = "ghc-" ++ p ++ "-devel"

dependencies :: PackageDescription  -- ^pkg description
                -> IO ([String], [String], [String], [String])
                -- ^depends, tools, c-libs, pkgcfg
dependencies pkgDesc = do
    let self = packageName $ package pkgDesc
        deps = buildDependencies pkgDesc self
        buildinfo = allBuildInfo pkgDesc
        tools =  nub $ map exeDepName (concatMap buildTools buildinfo)
        pkgcfgs = nub $ map pkgcfgDepName $ concatMap pkgconfigDepends buildinfo
        clibs = nub $ concatMap extraLibs buildinfo
    return (deps, delete self tools, clibs, pkgcfgs)

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
    putStrLn $ "Running repoquery on" +-+ "lib" ++ lib
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

warning :: String -> IO ()
warning s = hPutStrLn stderr $ "Warning:" +-+ s

packageDependencies :: Bool   -- ^strict mode: True means abort on unknown dependencies
                -> PackageDescription  -- ^pkg description
                -> IO ([String], [String], [String], [String])
                -- ^depends, tools, c-libs, pkgcfg
packageDependencies strict pkgDesc = do
    (deps, tools', clibs', pkgcfgs) <- dependencies pkgDesc
    let excludedTools n = n `notElem` ["ghc", "hsc2hs", "perl"]
        mapTools "gtk2hsC2hs" = "gtk2hs-buildtools"
        mapTools "gtk2hsHookGenerator" = "gtk2hs-buildtools"
        mapTools "gtk2hsTypeGen" = "gtk2hs-buildtools"
        mapTools tool = tool
        tools = filter excludedTools $ nub $ map mapTools tools'
    clibsWithErrors <- mapM resolveLib clibs'
    when (any isNothing clibsWithErrors) $
      if strict
      then fail "cannot resolve all clib dependencies"
      else putStrLn "Warning: could not resolve all clib dependencies"
    let clibs = catMaybes clibsWithErrors
    let showPkgCfg p = "pkgconfig(" ++ p ++ ")"
    return (map showDep deps, tools, nub clibs, map showPkgCfg pkgcfgs)

testsuiteDependencies :: PackageDescription  -- ^pkg description
                -> String           -- ^self
                -> [String]         -- ^depends
testsuiteDependencies pkgDesc self =
  map showDep . delete self . filter excludedPkgs . nub . map depName $ concatMap (targetBuildDepends . testBuildInfo) (testSuites pkgDesc)

missingPackages :: PackageDescription -> IO [String]
missingPackages pkgDesc = do
  (deps, tools, clibs, pkgcfgs) <- packageDependencies False pkgDesc
  pcpkgs <- mapM derefPkg pkgcfgs
  filterM notInstalled $ deps ++ ["ghc-Cabal-devel", "ghc-rpm-macros"] ++ tools ++ clibs ++ pcpkgs

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

subPackages :: Maybe FilePath -> PackageDescription -> IO [String]
subPackages mspec pkgDesc = do
  develSubpkgs <- filter ("-devel" `isSuffixOf`) <$> maybe (return []) (rpmspec [] (Just "%{name}")) mspec
  let self = packageName $ package pkgDesc
  return $ delete (showDep self) develSubpkgs

pkgInstallMissing :: PackageData -> Bool -> IO ()
pkgInstallMissing pkgdata hard = do
  let pkgDesc = packageDesc pkgdata
      mspec = specFilename pkgdata
  missing <- missingPackages pkgDesc
  unless (null missing) $ do
    subpkgs <- subPackages mspec pkgDesc
    let pkgs = missing \\ subpkgs
    pkginstaller <- packageManager
    putStrLn $ "Running repoquery" +-+ unwords pkgs
    repopkgs <- filter (/= "") <$> mapM (repoquery ["--qf", "%{name}"]) pkgs
    let missing' = pkgs \\ repopkgs
    if missing' /= [] && hard
      then error $ unwords missing' +-+ "not available."
      else do
      unless (null missing') $ do
        putStrLn "Unavailable dependencies:"
        mapM_ putStrLn missing'
      unless (null repopkgs) $ do
        putStrLn "Uninstalled dependencies:"
        mapM_ putStrLn repopkgs
        uid <- getEffectiveUserID
        maybeSudo <-
          if uid == 0
          then return Nothing
          else do
            havesudo <- optionalProgram "sudo"
            return $ if havesudo then Just "sudo" else Nothing
        let args = map showPkg repopkgs
        putStrLn $ "Running:" +-+ fromMaybe "" maybeSudo +-+ pkginstaller +-+ "install" +-+ unwords args
        let exec = if hard then cmd_ else trySystem
        fedora <- rpmEval "%fedora"
        let nogpgcheck = ["--nogpgcheck" | fedora `elem` []]
        exec (fromMaybe pkginstaller maybeSudo) $ maybe [] (const [pkginstaller]) maybeSudo ++ ("install" : args ++ nogpgcheck)
          where
            showPkg :: String -> String
            showPkg p = if '(' `elem` p then show p else p
