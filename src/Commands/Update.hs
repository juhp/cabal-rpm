-- |
-- Module      :  Commands.Update
-- Copyright   :  (C) 2014-2017  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
--
-- Explanation: update spec file to a new package version

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Commands.Update (
  update
  ) where

import Commands.Spec (createSpecFile)
import Distro (detectDistro, Distro(..))
import FileUtils (withTempDirectory)
import Options (RpmFlags (..))
import PackageUtils (PackageData (..), bringTarball, editSpecField,
                     getRevisedCabal, latestPackage, packageName, packageVersion,
                     patchSpec, prepare, removePrefix, rwGitDir)
import SysCmd (cmd_, die, grep_, (+-+))
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (unless, when)
import Data.Maybe (isJust)
import Distribution.PackageDescription (PackageDescription (..))
import System.Directory (createDirectory, setCurrentDirectory)
import System.FilePath ((</>), (<.>))

update :: PackageData -> RpmFlags -> Maybe String -> IO ()
update pkgdata flags mpkgver =
  case specFilename pkgdata of
    Nothing -> die "No (unique) .spec file in directory."
    Just spec -> do
      let pkgDesc = packageDesc pkgdata
          pkg = package pkgDesc
          name = packageName pkg
          ver = packageVersion pkg
          current = name ++ "-" ++ ver
          revised = isJust $ lookup "x-revision" (customFieldsPD pkgDesc)
      latest <- case mpkgver of
                  Just pv -> return pv
                  Nothing -> latestPackage (rpmStream flags) name
      let updated = current /= latest
      getRevisedCabal latest
      unless updated $
        putStrLn $ current +-+ "is already latest version."
      when (not revised || updated) $ do
        rwGit <- rwGitDir
        when updated $ do
          bringTarball latest False
          when rwGit $
            cmd_ "fedpkg" ["new-sources", latest <.> "tar.gz"]
        withTempDirectory $ \cwd -> do
          curspec <- createSpecVersion current spec revised
          newspec <- createSpecVersion latest spec True
          let newver = removePrefix (name ++ "-") latest
          subpkg <- grep_ "%{subpkgs}" spec
          distro <- maybe detectDistro return (rpmDistribution flags)
          let suffix = if distro == SUSE then "" else "%{?dist}"
          if updated && not subpkg && not (rpmSubpackage flags)
            then editSpecField "Release" ("1" ++ suffix) $ cwd </> spec
            else editSpecField "Version" newver $ cwd </> spec
          patchSpec (Just cwd) curspec newspec
          setCurrentDirectory cwd
          when updated $ do
            unless (subpkg || rpmSubpackage flags) $
              if distro == SUSE
              then editSpecField "Release" ("1") spec
              else do
                editSpecField "Release" ("0" ++ suffix) spec
                cmd_ "rpmdev-bumpspec" ["-c", "update to" +-+ newver, spec]
            when rwGit $
              cmd_ "git" ["commit", "-a", "-m", "update to" +-+ newver]
  where
    createSpecVersion :: String -> String -> Bool -> IO FilePath
    createSpecVersion pkgver spec revise = do
      pkgdata' <- prepare flags (Just pkgver) revise
      let pkgdata'' = pkgdata' { specFilename = Just spec }
          dir = pkgver <.> if revise then "" else "orig"
      createDirectory dir
      createSpecFile pkgdata'' flags (Just dir)
