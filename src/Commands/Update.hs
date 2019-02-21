-- |
-- Module      :  Commands.Update
-- Copyright   :  (C) 2014-2019  Jens Petersen
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
import Distro (defaultRelease, detectDistro, Distro(..))
import FileUtils (withTempDirectory)
import Options (RpmFlags (..))
import PackageUtils (PackageData (..), bringTarball, editSpecField,
                     getRevisedCabal, getSpecField, latestPackage, packageName,
                     packageVersion, patchSpec, prepare, readVersion,
                     removePrefix, removeSuffix)
import SimpleCmd (cmd_, grep_, shell_, (+-+))
import SimpleCmd.Git (rwGitDir)
import SysCmd (die)
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (unless, when)
import Data.Maybe (isJust)
import Distribution.PackageDescription (PackageDescription (..))
import System.Directory (createDirectory, renameFile, setCurrentDirectory)
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
          curVer = readVersion ver
          current = name ++ "-" ++ ver
          revised = isJust $ lookup "x-revision" (customFieldsPD pkgDesc)
      latest <- case mpkgver of
                    Just pv -> return pv
                    Nothing -> latestPackage (rpmStream flags) name
      let newver = removePrefix (name ++ "-") latest
      let latestVer = readVersion newver
          updated = latestVer > curVer
      if latestVer < curVer
        then putStrLn $ "current" +-+ ver +-+ "is newer!"
        else do
        getRevisedCabal latest
        unless updated $
          putStrLn "Package is already latest version."
        when (not revised || updated) $ do
          withTempDirectory $ \cwd -> do
            let specfile = cwd </> spec
            subpkg <- grep_ "%{subpkgs}" specfile
            (curspec, _) <- createSpecVersion current specfile revised subpkg
            (newspec, newrevised) <- createSpecVersion latest specfile True subpkg
            distro <- maybe detectDistro return (rpmDistribution flags)
            let suffix = if distro == SUSE then "" else "%{?dist}"
                defrelease = defaultRelease distro
            currel <- removeSuffix suffix <$> getSpecField "Release" specfile
            editSpecField "Release" (defrelease ++ suffix) specfile
            patchSpec (Just cwd) curspec newspec
            ver' <- getSpecField "Version" specfile
            when (ver' /= newver) $
              editSpecField "Version" newver specfile
            if updated && not subpkg
              then editSpecField "Release" (defrelease ++ suffix) specfile
              else editSpecField "Release" (currel ++ suffix) specfile
            when updated $ do
              when (distro /= SUSE) $ do
                -- FIXME reset when all subpkgs updated
                unless subpkg $
                  editSpecField "Release" ("0" ++ suffix) specfile
                cmd_ "rpmdev-bumpspec" ["-c", "update to" +-+ newver, specfile]
              setCurrentDirectory cwd
              rwGit <- rwGitDir
              when (rwGit && subpkg) $ do
                cmd_ "cp" ["-p", "sources", "sources.cblrpm"]
                cmd_ "sed" ["-i", "/" ++ current <.> "tar.gz" ++ "/d", "sources.cblrpm"]
              bringTarball latest False spec
              when rwGit $ do
                cmd_ "fedpkg" ["new-sources", latest <.> "tar.gz"]
                when subpkg $ do
                  shell_ $ "cat sources >>" +-+ "sources.cblrpm"
                  renameFile "sources.cblrpm" "sources"
                when newrevised $
                  cmd_ "git" ["add", latest <.> "cabal"]
                when revised $
                  cmd_ "git" ["rm", current <.> "cabal"]
                cmd_ "git" ["commit", "-a", "-m", "update to" +-+ newver]
  where
    createSpecVersion :: String -> String -> Bool -> Bool -> IO (FilePath, Bool)
    createSpecVersion pkgver spec revise subpkg = do
      let flags' = flags { rpmSubpackage = subpkg }
      pd <- prepare flags' (Just pkgver) revise
      let pkgdata' = pd { specFilename = Just spec }
          dir = pkgver <.> if revise then "" else "orig"
      createDirectory dir
      newspec <- createSpecFile pkgdata' flags' (Just dir)
      let newrevised =
            let pkgDesc = packageDesc pkgdata' in
              isJust $ lookup "x-revision" (customFieldsPD pkgDesc)
      return (newspec, newrevised)
