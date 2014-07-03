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
import Commands.RpmBuild (rpmBuild_, RpmStage (..))
import Commands.Spec (createSpecFile)

import PackageUtils (simplePackageDescription)
import Setup (parseArgs)

import Data.Maybe (listToMaybe, fromMaybe)
import System.Directory (removeDirectoryRecursive)
import System.Environment (getArgs)

main :: IO ()
main = do
    (opts, args) <- getArgs >>= parseArgs
    let (cmd:args') = args
        path = fromMaybe "." $ listToMaybe args'
    (cabalPath, pkgDesc, mtmp) <- simplePackageDescription path opts

    case cmd of
        "spec"        -> createSpecFile cabalPath pkgDesc opts Nothing
        "srpm"        -> rpmBuild_      cabalPath pkgDesc opts Source
        "prep"        -> rpmBuild_      cabalPath pkgDesc opts Prep
        "local"       -> rpmBuild_      cabalPath pkgDesc opts Binary
        "builddep"    -> rpmBuild_      cabalPath pkgDesc opts BuildDep
        "diff"        -> diff           cabalPath pkgDesc opts
        "install"     -> install        cabalPath pkgDesc opts
        "depends"     -> depends        pkgDesc
        "requires"    -> requires       pkgDesc
        "missingdeps" -> missingDeps    pkgDesc
        "rpm"         -> do
            putStrLn "* Warning the 'rpm' command has been renamed to 'local':"
            putStrLn "* this alias may be removed in a future release."
            rpmBuild_ cabalPath pkgDesc opts Binary

        c -> error $ "Unknown cmd: " ++ c

    maybe (return ()) removeDirectoryRecursive mtmp
