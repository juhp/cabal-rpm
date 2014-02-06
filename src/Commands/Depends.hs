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
    requires
    ) where

import Dependencies (dependencies, packageDependencies )
import PackageUtils (packageName, simplePackageDescription)
import Setup (RpmFlags (..))

import Data.List (sort)
import Distribution.PackageDescription (GenericPackageDescription (..),
                                        PackageDescription (..))

depends :: GenericPackageDescription -> RpmFlags -> IO ()
depends genPkgDesc flags = do
    pkgDesc <- simplePackageDescription genPkgDesc flags
    let pkg = package pkgDesc
        name = packageName pkg
    (deps, tools, clibs, pkgcfgs, _) <- dependencies pkgDesc name
    mapM_ putStrLn $ deps ++ tools ++ clibs ++ pkgcfgs

requires :: GenericPackageDescription -> RpmFlags -> IO ()
requires genPkgDesc flags = do
    pkgDesc <- simplePackageDescription genPkgDesc flags
    let pkg = package pkgDesc
        name = packageName pkg
    (deps, tools, clibs, pkgcfgs, _) <- packageDependencies pkgDesc name
    mapM_ putStrLn $ sort $ deps ++ tools ++ clibs ++ pkgcfgs
