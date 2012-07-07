-- |
-- Module      :  Distribution.Package.Rpm.Main
-- Copyright   :  Bryan O'Sullivan 2007
--
-- Maintainer  :  Bryan O'Sullivan <bos@serpentine.com>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Main entry point for building RPM packages.

-- This software may be used and distributed according to the terms of
-- the GNU General Public License, incorporated herein by reference.

module Distribution.Package.Rpm.Main where

import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Package.Rpm (rpm)
import Distribution.Package.Rpm.Setup (RpmFlags (..), parseArgs)
import Distribution.Simple.Utils (defaultPackageDesc)
import System.Environment (getArgs)

main :: IO ()

main = do opts <- getArgs >>= parseArgs
          let verbosity = rpmVerbosity opts
          descPath <- defaultPackageDesc verbosity
          pkgDesc <- readPackageDescription verbosity descPath
          rpm pkgDesc opts
