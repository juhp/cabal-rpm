-- |
-- Module      :  PackageUtils
-- Copyright   :  Jens Petersen 2013
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: functions related to Cabal dependency generation.

-- This software may be used and distributed according to the terms of
-- the GNU General Public License, incorporated herein by reference.

module PackageUtils (
  buildDependencies,
  depName,
  packageName,
  packageVersion,
  simplePackageDescription,
  showDep
    ) where

import Setup (RpmFlags (..))

import Data.List     (nub)
import Data.Version     (showVersion)

import Distribution.Compiler (CompilerFlavor (..))

import Distribution.Package  (Dependency (..), PackageIdentifier (..),
                              PackageName (..))
import Distribution.PackageDescription (GenericPackageDescription (..),
                                        PackageDescription (..))
import Distribution.PackageDescription.Configuration (finalizePackageDescription)

import Distribution.Simple.Compiler (Compiler (..))
import Distribution.Simple.Configure (configCompiler)
--import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.Program   (defaultProgramConfiguration)
import Distribution.Simple.Utils (die)

import Distribution.System            (Platform (..), buildArch, buildOS)

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
packageVersion pkg = (showVersion . pkgVersion) pkg

-- returns list of deps and whether package is self-dependent
buildDependencies :: PackageDescription -> String -> ([String], Bool)
buildDependencies pkgDesc self =
  let deps = nub $ map depName (buildDepends pkgDesc)
      excludedPkgs n = notElem n $ [self, "Cabal", "base", "ghc-prim", "integer-gmp"] in
  (filter excludedPkgs deps, elem self deps)

depName :: Dependency -> String
depName (Dependency (PackageName n) _) = n

showDep :: String -> String
showDep p = "ghc-" ++ p ++ "-devel"
