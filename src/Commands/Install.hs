-- |
-- Module      :  Commands.Install
-- Copyright   :  (C) 2012-2018 Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: cabal wrapper which installs rpm dependencies

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Commands.Install (
    install
    ) where

import Commands.RpmBuild (rpmBuild)
import Dependencies (missingPackages, notInstalled, pkgInstallMissing)
import Options (RpmFlags (..))
import PackageUtils (PackageData (..), rpmInstall, RpmStage (..), stripPkgDevel)
import SimpleCmd (cmd_, (+-+))
import SimpleCmd.Rpm (rpmspec)
import SysCmd (rpmEval)

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (filterM, unless, when)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

install :: PackageData -> RpmFlags -> IO ()
install pkgdata flags = do
  let pkgDesc = packageDesc pkgdata
  pkgInstallMissing pkgdata False
  stillMissing <- missingPackages pkgDesc
  unless (null stillMissing) $ do
    putStrLn $ "Missing:" +-+ unwords stillMissing
    mapM_ cblrpmInstallMissing stillMissing
  spec <- rpmBuild pkgdata flags Binary
  rpmdir <- rpmEval "%{_rpmdir}"
  rpms <- rpmspec [] (Just $ rpmdir </> "%{arch}/%{name}-%{version}-%{release}.rpm") spec
  -- metapkgs don't have base package
  filterM doesFileExist rpms >>= rpmInstall

cblrpmInstallMissing :: String -> IO ()
cblrpmInstallMissing pkg = do
  noPkg <- notInstalled pkg
  when noPkg $ do
    let dep = stripPkgDevel pkg
    putStrLn $ "Running cabal-rpm install" +-+ dep
    cmd_ "cabal-rpm" ["install", dep]
