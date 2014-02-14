-- |
-- Module      :  Commands.Install
-- Copyright   :  (C) 2012-2014 Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: cabal wrapper which yum installs dependencies

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Commands.Install (
    install
    ) where

import PackageUtils (missingPackages, packageName)
import SysCmd (runSystem, yumInstall)

import Distribution.PackageDescription (PackageDescription (..))
import System.Directory (setCurrentDirectory)
import System.FilePath.Posix (takeDirectory)

install :: FilePath -> PackageDescription -> IO ()
install cabalPath pkgDesc = do
    let pkg = package pkgDesc
        name = packageName pkg
    missing <- missingPackages pkgDesc name
    yumInstall missing False
    let pkgDir = takeDirectory cabalPath
    setCurrentDirectory pkgDir
    runSystem "cabal install"
