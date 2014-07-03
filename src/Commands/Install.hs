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

import Commands.RpmBuild (rpmBuild, RpmStage (..))
import PackageUtils (missingPackages, notInstalled, packageName,
                     removePrefix, removeSuffix)
import Setup (RpmFlags (..))
import SysCmd (cmd, cmd_, sudo, yumInstall, (+-+))

import Control.Applicative ((<$>))
import Control.Monad (when)
import Distribution.PackageDescription (PackageDescription (..))
--import System.Directory (getCurrentDirectory, setCurrentDirectory)
--import System.FilePath (takeDirectory)
import System.FilePath ((</>))

install :: FilePath -> PackageDescription -> RpmFlags -> IO ()
install cabalPath pkgDesc flags = do
    let pkg = package pkgDesc
        name = packageName pkg
    missing <- missingPackages pkgDesc name
    yumInstall missing False
    stillMissing <- missingPackages pkgDesc name
    putStrLn $ "Missing:" +-+ unwords stillMissing
    mapM_ installMissing stillMissing
--    let pkgDir = takeDirectory cabalPath
    spec <- rpmBuild cabalPath pkgDesc flags Binary
    arch <- cmd "arch" []
    rpms <- (map (\ p -> arch </> p ++ ".rpm") . lines) <$>
            cmd "rpmspec" ["-q", spec]
    sudo "yum" $ ["-y", "localinstall"] ++ rpms

installMissing :: String -> IO ()
installMissing pkg = do
  noInstall <- notInstalled pkg
  when noInstall $ do
    let dep = removeSuffix "-devel" $ removePrefix "ghc-" pkg
    putStrLn $ "Running cblrpm install" +-+ dep
    cmd_ "cblrpm" ["install", dep]
