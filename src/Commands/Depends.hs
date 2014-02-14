-- |
-- Module      :  Commands.Depends
-- Copyright   :  (C) 2014 Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
--
-- Explanation: cabal wrapper which yum installs dependencies

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Commands.Depends (
    depends,
    missingDeps,
    requires
    ) where

import Dependencies (dependencies, packageDependencies )
import PackageUtils (missingPackages, packageName)

import Data.List (sort)
import Distribution.PackageDescription (PackageDescription (..))

depends :: PackageDescription -> IO ()
depends pkgDesc = do
    let pkg = package pkgDesc
        name = packageName pkg
    (deps, tools, clibs, pkgcfgs, _) <- dependencies pkgDesc name
    mapM_ putStrLn $ deps ++ tools ++ clibs ++ pkgcfgs

requires :: PackageDescription -> IO ()
requires pkgDesc = do
    let pkg = package pkgDesc
        name = packageName pkg
    (deps, tools, clibs, pkgcfgs, _) <- packageDependencies pkgDesc name
    mapM_ putStrLn $ sort $ deps ++ tools ++ clibs ++ pkgcfgs

missingDeps :: PackageDescription -> IO ()
missingDeps pkgDesc = do
    let pkg = package pkgDesc
        name = packageName pkg
    missing <- missingPackages pkgDesc name
    mapM_ putStrLn missing
