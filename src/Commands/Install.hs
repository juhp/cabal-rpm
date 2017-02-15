-- |
-- Module      :  Commands.Install
-- Copyright   :  (C) 2012-2016 Jens Petersen
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
import Dependencies (missingPackages, notInstalled)
import Options (RpmFlags (..))
import PackageUtils (PackageData (..), RpmStage (..), stripPkgDevel)
import SysCmd (cmd, cmd_, pkgInstall, rpmInstall, (+-+))

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
  missing <- missingPackages pkgDesc
  unless (null missing) $ do
    pkgInstall missing False
    stillMissing <- missingPackages pkgDesc
    unless (null stillMissing) $ do
      putStrLn $ "Missing:" +-+ unwords stillMissing
      mapM_ installMissing stillMissing
  spec <- rpmBuild pkgdata flags Binary
  arch <- cmd "arch" []
  rpmdir <- cmd "rpm" ["--eval", "%{_rpmdir}"]
  rpms <- (map (\ p -> rpmdir </> arch </> p ++ ".rpm") . lines) <$>
          cmd "rpmspec" ["-q", spec]
  -- metapkgs don't have base package
  filterM doesFileExist rpms >>= rpmInstall

installMissing :: String -> IO ()
installMissing pkg = do
  noInstall <- notInstalled pkg
  when noInstall $ do
    let dep = stripPkgDevel pkg
    putStrLn $ "Running cblrpm install" +-+ dep
    cmd_ "cblrpm" ["install", dep]
