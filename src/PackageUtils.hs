-- |
-- Module      :  PackageUtils
-- Copyright   :  (C) 2013-2014  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: functions related to Cabal dependency generation.

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module PackageUtils (
  isScmDir,
  missingPackages,
  packageName,
  packageVersion,
  simplePackageDescription
    ) where

import Dependencies (packageDependencies)
import Setup (RpmFlags (..))
import SysCmd (systemBool, (+-+))

import Control.Monad    (filterM, liftM)
import Data.Version     (showVersion)

import Distribution.Compiler (CompilerFlavor (..))
import Distribution.Package  (PackageIdentifier (..),
                              PackageName (..))
import Distribution.PackageDescription (GenericPackageDescription (..),
                                        PackageDescription (..))
import Distribution.PackageDescription.Configuration (finalizePackageDescription)

import Distribution.Simple.Compiler (Compiler (..))
import Distribution.Simple.Configure (configCompiler)
import Distribution.Simple.Program   (defaultProgramConfiguration)
import Distribution.Simple.Utils (die)

import Distribution.System (Platform (..), buildArch, buildOS)

import System.Directory (doesDirectoryExist)
import System.FilePath.Posix ((</>))

simplePackageDescription :: GenericPackageDescription -> RpmFlags
                         -> IO PackageDescription
simplePackageDescription genPkgDesc flags = do
    (compiler, _) <- configCompiler (Just GHC) Nothing Nothing
                     defaultProgramConfiguration
                     (rpmVerbosity flags)
    case finalizePackageDescription (rpmConfigurationsFlags flags)
          (const True) (Platform buildArch buildOS) (compilerId compiler)
          [] genPkgDesc of
      Left e -> die $ "finalize failed: " ++ show e
      Right (pd, _) -> return pd

packageName :: PackageIdentifier -> String
packageName pkg = name
  where PackageName name = pkgName pkg

packageVersion :: PackageIdentifier -> String
packageVersion = showVersion . pkgVersion

(<||>) :: IO Bool -> IO Bool -> IO Bool
(<||>) f s = do
  one <- f
  if one then return True else s

isScmDir :: FilePath -> IO Bool
isScmDir dir =
  doesDirectoryExist (dir </> ".git") <||> doesDirectoryExist (dir </> "_darcs")

notInstalled :: String -> IO Bool
notInstalled br =
  liftM not $ systemBool $ "rpm -q --whatprovides" +-+ shellQuote br
  where
    shellQuote :: String -> String
    shellQuote (c:cs) = (if c `elem` "()" then (['\\', c] ++) else (c:)) (shellQuote cs)
    shellQuote "" = ""

missingPackages :: PackageDescription -> String -> IO [String]
missingPackages pkgDesc name = do
  (deps, tools, clibs, pkgcfgs, _) <- packageDependencies pkgDesc name
  filterM notInstalled $ deps ++ tools ++ clibs ++ pkgcfgs
