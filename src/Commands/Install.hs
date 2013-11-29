-- |
-- Module      :  Commands.Install
-- Copyright   :  Jens Petersen 2012-2013
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: cabal wrapper which yum installs dependencies

-- This software may be used and distributed according to the terms of
-- the GNU General Public License, incorporated herein by reference.

module Commands.Install (
    install
    ) where

import Depends (dependencies)
import PackageUtils (packageName, simplePackageDescription)
import Setup (RpmFlags (..))
import SysCmd (trySystem, systemBool, yumInstall, (+-+))

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
    yumInstall missing
    let pkgDir = takeDirectory cabalPath
    setCurrentDirectory pkgDir
    trySystem ("cabal install")
  where
    notInstalled :: String -> IO Bool
    notInstalled br = do
      liftM not $ systemBool $ "rpm -q --whatprovides" +-+ (shellQuote br)
    shellQuote :: String -> String
    shellQuote (c:cs) = (if (elem c "()") then (['\\', c] ++) else (c:)) (shellQuote cs)
    shellQuote "" = ""
