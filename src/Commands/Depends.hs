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

import Dependencies (dependencies, missingPackages, packageDependencies)
import PackageUtils (PackageData (..), prepare, repoquery)
import Types

import SimpleCabal (PackageIdentifier, PackageName)
import SimpleCmd ((+-+))

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (filterM, unless, void)
import Data.List (nub, sort, (\\))
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
      mapM_ putStrLn $ sort . nub  $ map (show . RpmHsLib Devel) (deps ++ setup) ++ tools ++ clibs ++ pkgcfgs
    Missing -> do
      missing <- missingPackages pkgDesc >>= filterM notAvail
      mapM_ (putStrLn . showDep) missing
      let miss = mapMaybe hsDep missing
      unless (null miss) $ putStrLn ""
      void $ recurseMissing flags stream [] miss
  where
    showDep :: RpmPackage -> String
    showDep (RpmHsLib _ n) = display n
    showDep (RpmHsBin n) = display n
    showDep p = show p

hsDep :: RpmPackage -> Maybe PackageName
hsDep (RpmHsLib _ n) = Just n
hsDep _ = Nothing

recurseMissing :: Flags -> Stream -> [PackageName] -> [PackageName] -> IO [PackageName]
recurseMissing _ _ already [] = return already
recurseMissing flags stream already (dep:deps) = do
  miss <- missingDepsPkg flags stream dep
  putMissing miss already
  let hmiss = mapMaybe hsDep miss
  let accum = nub $ hmiss ++ already
  -- deeper <- recurseMissing flags stream accum (miss \\ accum)
  -- let accum2 = nub $ accum ++ deeper
  more <- recurseMissing flags stream accum (deps \\ accum)
  return $ nub $ accum ++ more

notAvail :: RpmPackage -> IO Bool
notAvail pkg = null <$> repoquery [] (show pkg)

missingDepsPkg :: Flags -> Stream -> PackageName -> IO [RpmPackage]
missingDepsPkg flags stream pkg = do
  pkgdata <- prepare flags stream (Just (unversionedPkgId pkg)) False
  missingPackages (packageDesc pkgdata) >>= filterM notAvail

putMissing :: [RpmPackage] -> [PackageName] -> IO ()
putMissing [] _ = return ()
putMissing deps already = putStrLn $ "  " ++ "needs:" +-+ unwords (markAlready deps)
  where
    markAlready :: [RpmPackage] -> [String]
    markAlready [] = []
    markAlready (d:ds) =
      let (op, cl) = if maybe False (`elem` already) (hsDep d) then ("(", ")") else ("", "") in
      (op ++ show d ++ cl) : markAlready ds
