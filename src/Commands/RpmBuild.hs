-- |
-- Module      :  Commands.RpmBuild
-- Copyright   :  (C) 2007-2008  Bryan O'Sullivan
--                (C) 2012-2016  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Support for building RPM packages.  Can also generate
-- an RPM spec file if you need a basic one to hand-customize.

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Commands.RpmBuild (
    rpmBuild, rpmBuild_
    ) where

import Commands.Spec (createSpecFile)
import Dependencies (pkgInstallMissing)
import Options (RpmFlags (..))
import PackageUtils (copyTarball, PackageData (..), packageName,
                     packageVersion, rpmbuild, RpmStage (..))
import SysCmd (cmd, (+-+))

--import Control.Exception (bracket)
import Control.Monad    (unless, void, when)
import Distribution.PackageDescription (PackageDescription (..))

--import Distribution.Version (VersionRange, foldVersionRange')

import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- autoreconf :: Verbosity -> PackageDescription -> IO ()
-- autoreconf verbose pkgDesc = do
--     ac <- doesFileExist "configure.ac"
--     when ac $ do
--         c <- doesFileExist "configure"
--         when (not c) $ do
--             setupMessage verbose "Running autoreconf" pkgDesc
--             cmd_ "autoreconf" []

rpmBuild :: PackageData -> RpmFlags -> RpmStage ->
            IO FilePath
rpmBuild pkgdata flags stage = do
--  let verbose = rpmVerbosity flags
--  bracket (setFileCreationMask 0o022) setFileCreationMask $ \ _ -> do
--    autoreconf verbose pkgDesc
  let pkgDesc = packageDesc pkgdata
      mspec = specFilename pkgdata
  specFile <- maybe (createSpecFile pkgdata flags Nothing)
              (\ s -> putStrLn ("Using existing" +-+ s) >> return s)
              mspec
  let pkg = package pkgDesc
      name = packageName pkg
  when (stage `elem` [Binary,BuildDep]) $
    pkgInstallMissing pkgdata (stage == Binary)

  unless (stage == BuildDep) $ do
    srcdir <- cmd "rpm" ["--eval", "%{_sourcedir}"]
    let version = packageVersion pkg
        tarFile = srcdir </> name ++ "-" ++ version ++ ".tar.gz"

    tarFileExists <- doesFileExist tarFile
    unless tarFileExists $
      error "No tarball for source repo"

    copyTarball name version False srcdir

    rpmbuild stage False Nothing specFile
  return specFile

rpmBuild_ :: PackageData -> RpmFlags -> RpmStage -> IO ()
rpmBuild_ pkgdata flags stage =
  void (rpmBuild pkgdata flags stage)
