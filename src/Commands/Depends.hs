-- |
-- Module      :  Commands.Depends
-- Copyright   :  (C) 2014 Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
--
-- Explanation: cabal wrapper which yum installs dependencies

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Commands.Depends (
    depends,
    Depends (..)
    ) where

import Dependencies (dependencies, packageDependencies, warning)
import PackageUtils (missingPackages, PackageData (..), packageName,
                     stripPkgDevel)
import SysCmd (cmd, cmdQuiet, (+-+))

import Control.Applicative ((<$>))
import Control.Monad (filterM)
import Data.List (sort, (\\))
import Distribution.PackageDescription (PackageDescription (..))

data Depends = Depends | Requires | Missing

depends :: PackageData -> Depends -> IO ()
depends pkgdata action = do
  let pkgDesc = packageDesc pkgdata
      pkg = package pkgDesc
      name = packageName pkg
  case action of
    Depends -> do
      (deps, tools, clibs, pkgcfgs, _) <- dependencies pkgDesc name
      let clibs' = map (\ lib -> "lib" ++ lib ++ ".so") clibs
      let pkgcfgs' = map (++ ".pc") pkgcfgs
      mapM_ putStrLn $ deps ++ tools ++ clibs' ++ pkgcfgs'
    Requires -> do
      (deps, tools, clibs, pkgcfgs, _) <- packageDependencies pkgDesc name
      mapM_ putStrLn $ sort $ deps ++ tools ++ clibs ++ pkgcfgs
    Missing -> do
      missing <- missingPackages pkgDesc name >>= filterM notAvail
      warning $ name +-+ "misses" +-+ show missing
      mapM_ (recurseMissing "" missing . stripPkgDevel) missing
  where
    recurseMissing :: String -> [String] -> String -> IO ()
    recurseMissing indent others dep =
      -- FIXME really need to handle recursion internally
      if dep == "lens" && "lens" `notElem` others
        then putStrLn (indent ++ "lens")
        else do
        putStrLn $ indent ++ dep
        rmiss <- words <$> cmdQuiet "cblrpm" ["missingdeps", dep]
        warning $ dep +-+ "rmisses" +-+ show rmiss
        mapM_ (recurseMissing (indent ++ "  ") (rmiss ++ others)) (rmiss \\ others)

    notAvail :: String -> IO Bool
    notAvail pkg = null <$> cmd "repoquery" [pkg]
