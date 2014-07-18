-- |
-- Module      :  Dependencies
-- Copyright   :  (C) 2012-2014  Jens Petersen
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
  packageDependencies,
  showDep,
  testsuiteDependencies
  ) where

import SysCmd (cmd, optionalProgram, (+-+))

import Control.Applicative ((<$>))

import Data.List (delete, nub)
import Data.Maybe (catMaybes)

import Distribution.Package  (Dependency (..), PackageName (..))
import Distribution.PackageDescription (PackageDescription (..),
                                        allBuildInfo,
                                        BuildInfo (..),
                                        TestSuite (..))
import System.Directory (doesDirectoryExist, doesFileExist)
import System.IO (hPutStrLn, stderr)

excludedPkgs :: String -> Bool
excludedPkgs = flip notElem ["Cabal", "base", "ghc-prim", "integer-gmp"]

-- returns list of deps and whether package is self-dependent
buildDependencies :: PackageDescription -> String -> ([String], Bool)
buildDependencies pkgDesc self =
  let deps = nub $ map depName (buildDepends pkgDesc) in
  (filter excludedPkgs (delete self deps), self `elem` deps)

depName :: Dependency -> String
depName (Dependency (PackageName n) _) = n

showDep :: String -> String
showDep p = "ghc-" ++ p ++ "-devel"

dependencies :: PackageDescription  -- ^pkg description
                -> String           -- ^pkg name
                -> IO ([String], [String], [String], [String], Bool)
                -- ^depends, tools, c-libs, pkgcfg, selfdep
dependencies pkgDesc self = do
    let (deps, selfdep) = buildDependencies pkgDesc self
        buildinfo = allBuildInfo pkgDesc
        tools =  nub $ map depName (concatMap buildTools buildinfo)
        pkgcfgs = nub $ map depName $ concatMap pkgconfigDepends buildinfo
        clibs = concatMap extraLibs buildinfo
    return (deps, tools, nub clibs, pkgcfgs, selfdep)

resolveLib :: String -> IO (Maybe String)
resolveLib lib = do
  lib64 <- doesDirectoryExist "/usr/lib64"
  let libsuffix = if lib64 then "64" else ""
  let lib_path = "/usr/lib" ++ libsuffix ++ "/lib" ++ lib ++ ".so"
  libInst <- doesFileExist lib_path
  if libInst
    then rpmquery "rpm" lib_path
    else do
    haveRpqry <- optionalProgram "repoquery"
    if haveRpqry
      then do
      putStrLn $ "Running repoquery on" +-+ "lib" ++ lib
      rpmquery "repoquery" lib_path
      else do
      warning $ "Install yum-utils to resolve package that provides uninstalled" +-+ lib_path
      return Nothing

-- use repoquery or rpm -q to query which package provides file
rpmquery :: String -> FilePath -> IO (Maybe String)
rpmquery c file = do
  out <- cmd c ["-q", "--qf=%{name}", "-f", file]
  let pkgs = nub $ words out
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

packageDependencies :: PackageDescription  -- ^pkg description
                -> String           -- ^pkg name
                -> IO ([String], [String], [String], [String], Bool)
                -- ^depends, tools, c-libs, pkgcfg, selfdep
packageDependencies pkgDesc self = do
    (deps, tools', clibs', pkgcfgs, selfdep) <- dependencies pkgDesc self
    let excludedTools n = n `notElem` ["ghc", "hsc2hs", "perl"]
        mapTools "gtk2hsC2hs" = "gtk2hs-buildtools"
        mapTools "gtk2hsHookGenerator" = "gtk2hs-buildtools"
        mapTools "gtk2hsTypeGen" = "gtk2hs-buildtools"
        mapTools tool = tool
        chrpath = ["chrpath" | selfdep]
        tools = filter excludedTools $ nub $ map mapTools tools' ++ chrpath
    clibs <- catMaybes <$> mapM resolveLib clibs'
    let showPkgCfg p = "pkgconfig(" ++ p ++ ")"
    return (map showDep deps, tools, clibs, map showPkgCfg pkgcfgs, selfdep)

testsuiteDependencies :: PackageDescription  -- ^pkg description
                -> String           -- ^self
                -> [String]         -- ^depends
testsuiteDependencies pkgDesc self =
  map showDep . delete self . filter excludedPkgs . map depName . nub $ concatMap targetBuildDepends $ map testBuildInfo $ testSuites pkgDesc

