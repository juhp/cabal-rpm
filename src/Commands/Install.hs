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

import Dependencies (dependencies)
import PackageUtils (packageName, simplePackageDescription)
import Setup (RpmFlags (..))
import SysCmd (runSystem, systemBool, yumInstall, (+-+))

import Control.Monad    (filterM, liftM)

import Distribution.PackageDescription (GenericPackageDescription (..),
                                        PackageDescription (..))
import System.Directory (setCurrentDirectory)
import System.FilePath.Posix (takeDirectory)

install :: FilePath -> GenericPackageDescription -> RpmFlags -> IO ()
install cabalPath genPkgDesc flags = do
    pkgDesc <- simplePackageDescription genPkgDesc flags
    let pkg = package pkgDesc
        name = packageName pkg
    (deps, tools, clibs, pkgcfgs, _) <- dependencies pkgDesc name
    missing <- filterM notInstalled $ deps ++ tools ++ clibs ++ pkgcfgs
    yumInstall missing False
    let pkgDir = takeDirectory cabalPath
    setCurrentDirectory pkgDir
    runSystem "cabal install"
  where
    notInstalled :: String -> IO Bool
    notInstalled br =
      liftM not $ systemBool $ "rpm -q --whatprovides" +-+ shellQuote br
    shellQuote :: String -> String
    shellQuote (c:cs) = (if c `elem` "()" then (['\\', c] ++) else (c:)) (shellQuote cs)
    shellQuote "" = ""
