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
import PackageUtils (PackageData (..), bringTarball, isGitDir, latestPackage,
                     packageName, packageVersion, patchSpec, prepare,
                     removePrefix)
import SysCmd (cmd_, die, grep_, (+-+))
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (unless, when)
import Distribution.PackageDescription (PackageDescription (..))
import System.Directory (createDirectory, getCurrentDirectory,
                         setCurrentDirectory)

update :: PackageData -> RpmFlags -> Maybe String -> IO ()
update pkgdata flags mpkgver =
  case specFilename pkgdata of
    Nothing -> die "No (unique) .spec file in directory."
    Just spec -> do
      let pkg = package $ packageDesc pkgdata
          name = packageName pkg
          ver = packageVersion pkg
          current = name ++ "-" ++ ver
      latest <- case mpkgver of
                  Just pv -> return pv
                  Nothing -> latestPackage (rpmHackage flags) name
      if current == latest
        then putStrLn $ current +-+ "is already latest version."
        else do
        bringTarball latest
        gitDir <- getCurrentDirectory >>= isGitDir
        rwGit <- if gitDir then grep_ "url = ssh://" ".git/config" else return False
        when rwGit $
            cmd_ "fedpkg" ["new-sources", latest ++ ".tar.gz"]
        withTempDirectory $ \cwd -> do
          curspec <- createSpecVersion current spec
          newspec <- createSpecVersion latest spec
          patchSpec (Just cwd) curspec newspec
          setCurrentDirectory cwd
          distro <- maybe detectDistro return (rpmDistribution flags)
          let suffix = if distro == SUSE then "" else "%{?dist}"
          subpkg <- grep_ "%{subpkgs}" spec
          unless (subpkg || rpmSubpackage flags) $
            cmd_ "sed" ["-i", "-e s/^\\(Release:        \\).*/\\10" ++ suffix ++ "/", spec]
          let newver = removePrefix (name ++ "-") latest
          if distro == SUSE
            then cmd_ "sed" ["-i", "-e s/^\\(Version:        \\).*/\\1" ++ newver ++ "/", spec]
            else cmd_ "rpmdev-bumpspec" ["-c", "update to" +-+ newver, spec]
          when rwGit $
            cmd_ "git" ["commit", "-a", "-m", "update to" +-+ newver]
  where
    createSpecVersion :: String -> String -> IO FilePath
    createSpecVersion ver spec = do
      pkgdata' <- prepare flags (Just ver)
      let pkgdata'' = pkgdata' { specFilename = Just spec }
      createDirectory ver
      createSpecFile pkgdata'' flags (Just ver)
