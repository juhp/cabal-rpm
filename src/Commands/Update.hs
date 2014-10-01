-- |
-- Module      :  Commands.Update
-- Copyright   :  (C) 2014  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
--
-- Explanation: update spec file to a new package version

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Commands.Update (
  update
  ) where

import Commands.Spec (createSpecFile)
import FileUtils (mktempdir)
import PackageUtils (PackageData (..), latestPkg, packageName, packageVersion,
                    prepare)
import Setup (RpmFlags (..))
import SysCmd ((+-+), shell)

import Distribution.PackageDescription (PackageDescription (..))
import Distribution.Simple.Utils (die)
                                        
import System.Directory (createDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))

update :: PackageData -> RpmFlags -> IO ()
update pkgdata flags =
  case specFilename pkgdata of
    Nothing -> die "No (unique) .spec file in directory."
    Just spec -> do
      tmpdir <- mktempdir
      (curspec, name) <- createSpecVersion pkgdata tmpdir
      latest <- latestPkg name
      putStrLn latest
      pkgdata' <- prepare (Just latest) flags
      (newspec, _) <- createSpecVersion pkgdata' tmpdir
      shell $ "diff" +-+ "-u" +-+ curspec +-+ newspec
      removeDirectoryRecursive tmpdir
  where
    createSpecVersion :: PackageData -> FilePath -> IO (FilePath, String)
    createSpecVersion pd tmp = do
      let pkg = package $ packageDesc pd
          version = packageVersion pkg
          dir = tmp </> version
      createDirectory dir
      spec <- createSpecFile pd flags (Just dir)
      return (spec, packageName pkg)
