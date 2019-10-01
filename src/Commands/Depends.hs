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

import Dependencies (dependencies, ghcDep, LibPkgType(..), missingPackages,
                     packageDependencies)
import PackageUtils (PackageData (..), prepare, repoquery,
                     stripPkgDevel, unPackageName)
import Types

import SimpleCmd ((+-+))

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (filterM, unless, void)
import Data.List (nub, sort, (\\))
import System.FilePath ((<.>))

data Depends = Depends | Requires | Missing

depends :: Depends -> Flags -> Stream -> Maybe Package -> IO ()
depends action flags stream mpkg = do
  pkgdata <- prepare flags stream mpkg False
  let pkgDesc = packageDesc pkgdata
  case action of
    Depends -> do
      let (deps, setup, tools, clibs, pkgcfgs) = dependencies pkgDesc
          clibs' = map (\ lib -> "lib" ++ lib <.> "so") clibs
          pkgcfgs' = map (<.> "pc") pkgcfgs
      mapM_ putStrLn $ map unPackageName (nub (deps ++ setup)) ++ tools ++ clibs' ++ pkgcfgs'
    Requires -> do
      (deps, setup, tools, clibs, pkgcfgs) <- packageDependencies pkgDesc
      mapM_ putStrLn $ sort . nub  $ map (ghcDep LibDevel) (deps ++ setup) ++ tools ++ clibs ++ pkgcfgs
    Missing -> do
      miss <- missingPackages pkgDesc >>= filterM notAvail
      let missing = map stripPkgDevel miss
      mapM_ putStrLn missing
      unless (null missing) $
        putStrLn ""
      void $ recurseMissing flags stream miss missing

recurseMissing :: Flags -> Stream -> [String] -> [String] -> IO [String]
recurseMissing _ _ already [] = return already
recurseMissing flags stream already (dep:deps) = do
  miss <- missingDepsPkg flags stream dep
  putMissing miss already
  let accum = nub $ miss ++ already
  deeper <- recurseMissing flags stream accum (miss \\ accum)
  let accum2 = nub $ accum ++ deeper
  more <- recurseMissing flags stream accum2 (deps \\ accum2)
  return $ nub $ accum2 ++ more

notAvail :: String -> IO Bool
notAvail pkg = null <$> repoquery [] pkg

missingDepsPkg :: Flags -> Stream -> Package -> IO [String]
missingDepsPkg flags stream pkg = do
  pkgdata <- prepare flags stream (Just pkg) False
  missingPackages (packageDesc pkgdata) >>= filterM notAvail

putMissing :: [String] -> [String] -> IO ()
putMissing [] _ = return ()
putMissing deps already = putStrLn $ "  " ++ "needs:" +-+ unwords (markAlready deps)
  where
    markAlready :: [String] -> [String]
    markAlready [] = []
    markAlready (d:ds) =
      let (op, cl) = if d `elem` already then ("(", ")") else ("", "") in
      (op ++ stripPkgDevel d ++ cl) : markAlready ds
