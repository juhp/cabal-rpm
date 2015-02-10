-- |
-- Module      :  Commands.Depends
-- Copyright   :  (C) 2014 Jens Petersen
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

import Dependencies (dependencies, missingPackages, packageDependencies)
import PackageUtils (PackageData (..), prepare, stripPkgDevel)
import Setup (quiet)
import SysCmd (cmd, (+-+))

import Control.Applicative ((<$>))
import Control.Monad (filterM, unless, void)
import Data.List (nub, sort, (\\))
import System.Directory	(removeDirectoryRecursive)

data Depends = Depends | Requires | Missing

depends :: PackageData -> Depends -> IO ()
depends pkgdata action = do
  let pkgDesc = packageDesc pkgdata
  case action of
    Depends -> do
      (deps, tools, clibs, pkgcfgs, _) <- dependencies pkgDesc
      let clibs' = map (\ lib -> "lib" ++ lib ++ ".so") clibs
      let pkgcfgs' = map (++ ".pc") pkgcfgs
      mapM_ putStrLn $ deps ++ tools ++ clibs' ++ pkgcfgs'
    Requires -> do
      (deps, tools, clibs, pkgcfgs, _) <- packageDependencies pkgDesc
      mapM_ putStrLn $ sort $ deps ++ tools ++ clibs ++ pkgcfgs
    Missing -> do
      miss <- missingPackages pkgDesc >>= filterM notAvail
      let missing = map stripPkgDevel miss
      mapM_ putStrLn missing
      unless (null missing) $
        putStrLn ""
      void $ recurseMissing miss missing

recurseMissing :: [String] -> [String] -> IO [String]
recurseMissing already [] = return already
recurseMissing already (dep:deps) = do
  miss <- missingDepsPkg dep
  putMissing dep miss already
  let accum = nub $ miss ++ already
  deeper <- recurseMissing accum (miss \\ accum)
  let accum2 = nub $ accum ++ deeper
  more <- recurseMissing accum2 (deps \\ accum2)
  return $ nub $ accum2 ++ more

notAvail :: String -> IO Bool
notAvail pkg = null <$> cmd "repoquery" [pkg]

missingDepsPkg :: String -> IO [String]
missingDepsPkg pkg = do
  pkgdata <- prepare (Just pkg) quiet
  maybe (return ()) removeDirectoryRecursive $ workingDir pkgdata
  missingPackages (packageDesc pkgdata) >>= filterM notAvail

putMissing :: String -> [String] -> [String] -> IO ()
putMissing _ [] _ = return ()
putMissing pkg deps already = putStrLn $ pkg +-+ "needs:" +-+ unwords (markAlready deps)
  where
    markAlready :: [String] -> [String]
    markAlready [] = []
    markAlready (d:ds) =
      let (op, cl) = if d `elem` already then ("(", ")") else ("", "") in
      (op ++ stripPkgDevel d ++ cl) : markAlready ds
