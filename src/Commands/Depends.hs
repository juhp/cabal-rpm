{-# LANGUAGE CPP #-}

-- |
-- Module      :  Commands.Depends
-- Copyright   :  (C) 2014-2019  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
--
-- Explanation: determines dependencies

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Commands.Depends (
    depends,
    Depends (..)
    ) where

import Dependencies (dependencies, hsDep, missingPackages, notAvail,
                     packageDependencies, recurseMissing, showDep)
import PackageUtils (PackageData (..), prepare)
import Types

import SimpleCabal (PackageIdentifier)

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (filterM, unless, void)
import Data.List (nub, sort)
import Data.Maybe (mapMaybe)
import Distribution.Text (display)
import System.FilePath ((<.>))

data Depends = Depends | Requires | Missing

depends :: Depends -> Flags -> Stream -> Maybe PackageIdentifier -> IO ()
depends action flags stream mpkgid = do
  pkgdata <- prepare flags stream mpkgid False
  let pkgDesc = packageDesc pkgdata
  case action of
    Depends -> do
      let (deps, setup, tools, clibs, pkgcfgs) = dependencies pkgDesc
          clibs' = map (\ lib -> "lib" ++ lib <.> "so") clibs
          pkgcfgs' = map (<.> "pc") pkgcfgs
      mapM_ putStrLn $ map display (nub (deps ++ setup)) ++ tools ++ clibs' ++ pkgcfgs'
    Requires -> do
      (deps, setup, tools, clibs, pkgcfgs) <- packageDependencies pkgDesc
      mapM_ putStrLn $ sort . nub  $ map (showRpm . RpmHsLib Devel) (deps ++ setup) ++ tools ++ clibs ++ pkgcfgs
    Missing -> do
      missing <- missingPackages pkgDesc >>= filterM notAvail
      mapM_ (putStrLn . showDep) missing
      let miss = mapMaybe hsDep missing
      unless (null miss) $ putStrLn ""
      void $ recurseMissing flags stream [] miss
