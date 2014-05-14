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
  dependencies, packageDependencies, showDep, testsuiteDependencies
  ) where

import SysCmd (tryReadProcess)

import Data.List        (nub)

import Distribution.Package  (Dependency (..), PackageName (..))
import Distribution.PackageDescription (PackageDescription (..),
                                        allBuildInfo,
                                        BuildInfo (..),
                                        TestSuite (..))

-- returns list of deps and whether package is self-dependent
buildDependencies :: PackageDescription -> String -> ([String], Bool)
buildDependencies pkgDesc self =
  let deps = nub $ map depName (buildDepends pkgDesc)
      excludedPkgs n = notElem n [self, "Cabal", "base", "ghc-prim", "integer-gmp"] in
  (filter excludedPkgs deps, self `elem` deps)

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

    clibs <- mapM repoqueryLib $ concatMap extraLibs buildinfo
    return (deps, tools, clibs, pkgcfgs, selfdep)

repoqueryLib :: String -> IO String
repoqueryLib lib = do
  let lib_path = "/usr/lib/lib" ++ lib ++ ".so"
  out <- tryReadProcess "repoquery" ["--qf=%{name}", "-qf", lib_path]
  let pkgs = nub $ words out
  case pkgs of
    [pkg] -> return pkg
    [] -> error $ "Could not resolve package that provides lib" ++ lib_path
    _ -> error $ "More than one package seems to provide lib" ++ lib_path ++ ": " ++ show pkgs

packageDependencies :: PackageDescription  -- ^pkg description
                -> String           -- ^pkg name
                -> IO ([String], [String], [String], [String], Bool)
                -- ^depends, tools, c-libs, pkgcfg, selfdep
packageDependencies pkgDesc self = do
    (deps, tools', clibs, pkgcfgs, selfdep) <- dependencies pkgDesc self
    let excludedTools n = n `notElem` ["ghc", "hsc2hs", "perl"]
        mapTools "gtk2hsC2hs" = "gtk2hs-buildtools"
        mapTools "gtk2hsHookGenerator" = "gtk2hs-buildtools"
        mapTools "gtk2hsTypeGen" = "gtk2hs-buildtools"
        mapTools tool = tool
        chrpath = ["chrpath" | selfdep]
        tools = filter excludedTools $ nub $ map mapTools tools' ++ chrpath

    let showPkgCfg p = "pkgconfig(" ++ p ++ ")"
    return (map showDep deps, tools, clibs, map showPkgCfg pkgcfgs, selfdep)

testsuiteDependencies :: PackageDescription  -- ^pkg description
                -> [String]         -- ^depends
testsuiteDependencies pkgDesc =
  map (showDep . depName) $ concatMap targetBuildDepends $ map testBuildInfo $ testSuites pkgDesc

