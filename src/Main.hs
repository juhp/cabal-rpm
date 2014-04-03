-- |
-- Module      :  Main
-- Copyright   :  (C) 2007  Bryan O'Sullivan
--                (C) 2012-2014  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Main entry point for building RPM packages.

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Main where

import Commands.Depends (depends, missingDeps, requires)
import Commands.Diff (diff)
import Commands.Install (install)
import Commands.RpmBuild (rpmBuild, RpmStage (..))
import Commands.Spec (createSpecFile)

import PackageUtils (simplePackageDescription)
import Setup (parseArgs)

import Data.Maybe (listToMaybe, fromMaybe)
import System.Directory (removeDirectoryRecursive)
import System.Environment (getArgs)

main :: IO ()
main = do (opts, args) <- getArgs >>= parseArgs
          let (cmd:args') = args
              path = fromMaybe "." $ listToMaybe args'
          (cabalPath, pkgDesc, mtmp) <- simplePackageDescription path opts
          case cmd of
               "spec" ->  createSpecFile cabalPath pkgDesc opts Nothing
               "srpm" ->  rpmBuild cabalPath pkgDesc opts Source
               "prep" ->  rpmBuild cabalPath pkgDesc opts Prep
               "local" -> rpmBuild cabalPath pkgDesc opts Binary
               "builddep" -> rpmBuild cabalPath pkgDesc opts BuildDep
               "install" -> install cabalPath pkgDesc
               "depends" -> depends pkgDesc
               "requires" -> requires pkgDesc
               "missingdeps" -> missingDeps pkgDesc
               "diff" -> diff cabalPath pkgDesc opts
               c -> error $ "Unknown cmd: " ++ c
          maybe (return ()) removeDirectoryRecursive mtmp

  -- where
  --   -- copied from Distribution.Simple.Configure configure
  --   depResolver = if build
  --                     then not . null . PackageIndex.lookupDependency pkgs'
  --                     else (const True)
  --       pkgs' = PackageIndex.insert internalPackage installedPackageSet
  --       pid = packageId genPkgDesc
  --       internalPackage = emptyInstalledPackageInfo {
  --               Installed.installedPackageId = InstalledPackageId $ display $ pid,
  --               Installed.sourcePackageId = pid
  --             }
  --           internalPackageSet = PackageIndex.fromList [internalPackage]

