-- |
-- Module      :  Commands.Install
-- Copyright   :  (C) 2012-2014 Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: cabal wrapper which yum installs dependencies

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Commands.Install (
    install
    ) where

import Commands.RpmBuild (rpmBuild)
import PackageUtils (missingPackages, notInstalled, PackageData (..),
                     packageName, RpmStage (..), stripPkgDevel)
import Setup (RpmFlags (..))
import SysCmd (cmd, cmd_, sudo, yumInstall, (+-+))

import Control.Applicative ((<$>))
import Control.Monad (when)
import Distribution.PackageDescription (PackageDescription (..))
--import System.Directory (getCurrentDirectory, setCurrentDirectory)
--import System.FilePath (takeDirectory)
import System.FilePath ((</>))

install :: PackageData -> RpmFlags -> IO ()
install pkgdata flags = do
  let pkgDesc = packageDesc pkgdata
      pkg = package pkgDesc
      name = packageName pkg
  missing <- missingPackages pkgDesc name
  yumInstall missing False
  stillMissing <- missingPackages pkgDesc name
  putStrLn $ "Missing:" +-+ unwords stillMissing
  mapM_ installMissing stillMissing
--  let pkgDir = takeDirectory cabalPath
  spec <- rpmBuild pkgdata flags Binary
  arch <- cmd "arch" []
  rpms <- (map (\ p -> arch </> p ++ ".rpm") . lines) <$>
          cmd "rpmspec" ["-q", spec]
  sudo "yum" $ ["-y", "localinstall"] ++ rpms

installMissing :: String -> IO ()
installMissing pkg = do
  noInstall <- notInstalled pkg
  when noInstall $ do
    let dep = stripPkgDevel pkg
    putStrLn $ "Running cblrpm install" +-+ dep
    cmd_ "cblrpm" ["install", dep]
