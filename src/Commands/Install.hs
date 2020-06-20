{-# LANGUAGE CPP #-}

-- |
-- Module      :  Commands.Install
-- Copyright   :  (C) 2012-2020 Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: installs rpms of package and dependencies

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Commands.Install (
    install
    ) where

import Commands.RpmBuild (rpmBuild, rpmBuild_)
import Dependencies (notInstalled, pkgInstallMissing)
import FileUtils (withTempDirectory)
import PackageUtils (checkForSpecFile, rpmInstall, RpmStage (..))
import SysCmd (rpmEval)
import Types

import SimpleCabal(PackageName)
import SimpleCmd (cmd_, (+-+))
import SimpleCmd.Rpm (rpmspec)

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
--import Control.Applicative ((<$>))
#endif
import Control.Monad (unless, when)
import Distribution.Text (display)
import System.FilePath ((</>))

install :: Flags -> PackageType -> Bool -> Maybe PackageVersionSpecifier -> IO ()
install flags pkgtype subpackage mpvs = do
  stillMissing <- pkgInstallMissing flags mpvs
  unless (null stillMissing) $ do
    putStrLn $ "Missing:" +-+ unwords (map display stillMissing)
    mapM_ cblrpmInstallMissing stillMissing
  mspec <- checkForSpecFile (pvsPackage =<< mpvs)
  case mspec of
    Nothing ->
      withTempDirectory $ \ _ -> do
      spec <- rpmBuild Binary False flags pkgtype subpackage mpvs
      rpmdir <- rpmEval "%{_rpmdir}"
      rpmspec [] (fmap (</> "%{arch}/%{name}-%{version}-%{release}.%{arch}.rpm") rpmdir) spec >>= rpmInstall False
    Just spec -> do
      rpmBuild_ Binary False flags pkgtype subpackage mpvs
      rpmdir <- rpmEval "%{_rpmdir}"
      rpmspec [] (fmap (</> "%{arch}/%{name}-%{version}-%{release}.%{arch}.rpm") rpmdir) spec >>= rpmInstall False

cblrpmInstallMissing :: PackageName -> IO ()
cblrpmInstallMissing pkg = do
  noPkg <- notInstalled $ RpmHsLib Prof pkg
  when noPkg $ do
    let dep = display pkg
    withTempDirectory $ \ _ -> do
      putStrLn $ "Running cabal-rpm install" +-+ dep
      cmd_ "cabal-rpm" ["install", dep]
