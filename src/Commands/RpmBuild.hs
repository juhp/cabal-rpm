-- |
-- Module      :  Commands.RpmBuild
-- Copyright   :  (C) 2007-2008  Bryan O'Sullivan
--                (C) 2012-2018  Jens Petersen
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
import FileUtils (mktempdir)
import Options (RpmFlags (..))
import PackageUtils (bringTarball, PackageData (..), packageName,
                     packageVersion, rpmbuild, RpmStage (..))
import SimpleCmd ((+-+))

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif
--import Control.Exception (bracket)
import Control.Monad (when)
import Data.Maybe (isNothing)
import Distribution.PackageDescription (PackageDescription (..))
import System.Directory (removeDirectoryRecursive)

--import Distribution.Version (VersionRange, foldVersionRange')

-- autoreconf :: Verbosity -> PackageDescription -> IO ()
-- autoreconf verbose pkgDesc = do
--     ac <- doesFileExist "configure.ac"
--     when ac $ do
--         c <- doesFileExist "configure"
--         when (not c) $ do
--             setupMessage verbose "Running autoreconf" pkgDesc
--             cmd_ "autoreconf" []

rpmBuild :: PackageData -> RpmFlags -> RpmStage ->
            IO (FilePath, Maybe FilePath)
rpmBuild pkgdata flags stage = do
  when (stage == Binary) $
    pkgInstallMissing flags pkgdata

  let pkgDesc = packageDesc pkgdata
      mspec = specFilename pkgdata
  mtmp <- if isNothing mspec
          then Just <$> mktempdir
          else return Nothing
  specFile <- maybe (createSpecFile pkgdata flags mtmp)
              (\ s -> putStrLn ("Using existing" +-+ s) >> return s)
              mspec
  let pkg = package pkgDesc
      name = packageName pkg
      version = packageVersion pkg
  bringTarball (name ++ "-" ++ version) True specFile
  rpmbuild stage specFile

  return (specFile, mtmp)

rpmBuild_ :: PackageData -> RpmFlags -> RpmStage -> IO ()
rpmBuild_ pkgdata flags stage = do
  (_, mtmpdir) <- rpmBuild pkgdata flags stage
  maybe (return ()) removeDirectoryRecursive mtmpdir
