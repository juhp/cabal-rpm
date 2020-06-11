{-# LANGUAGE CPP #-}

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
import Dependencies (pkgInstallMissing')
import PackageUtils (bringTarball, PackageData (..), prepare,
                     rpmbuild, RpmStage (..))
import Types

import SimpleCabal (package)
import SimpleCmd ((+-+))

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
--import Control.Applicative ((<$>))
#endif
import Control.Monad (void, when)
import Distribution.Verbosity (normal)

rpmBuild :: RpmStage -> Flags -> PackageType -> Bool
         -> Maybe PackageVersionSpecifier -> IO FilePath
rpmBuild stage flags pkgtype subpackage mpvs = do
  pkgdata <- prepare flags mpvs True False
  when (stage == Binary) $
    void $ pkgInstallMissing' pkgdata
  let pkgDesc = packageDesc pkgdata
      mspec = specFilename pkgdata
  specFile <- maybe (createSpecFile False normal flags False False pkgtype (if subpackage then Just Nothing else Nothing) Nothing mpvs)
              (\ s -> putStrLn ("Using existing" +-+ s) >> return s)
              mspec
  let pkgid = package pkgDesc
  bringTarball pkgid True (Just specFile)
  rpmbuild stage specFile

  return specFile

rpmBuild_ :: RpmStage -> Flags -> PackageType -> Bool
          -> Maybe PackageVersionSpecifier -> IO ()
rpmBuild_ stage flags pkgtype subpackage mpvs =
  void $ rpmBuild stage flags pkgtype subpackage mpvs
