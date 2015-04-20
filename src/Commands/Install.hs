-- |
-- Module      :  Commands.Install
-- Copyright   :  (C) 2012-2015 Jens Petersen
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
import PackageUtils (PackageData (..), RpmStage (..), stripPkgDevel)
import Setup (RpmFlags (..))
import SysCmd (cmd, cmd_, pkgInstall, rpmInstall, (+-+))

import Control.Applicative ((<$>))
import Control.Monad (unless, when)
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
  rpmInstall rpms

installMissing :: String -> IO ()
installMissing pkg = do
  noInstall <- notInstalled pkg
  when noInstall $ do
    let dep = stripPkgDevel pkg
    putStrLn $ "Running cblrpm install" +-+ dep
    cmd_ "cblrpm" ["install", dep]
