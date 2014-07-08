-- |
-- Module      :  Commands.Diff
-- Copyright   :  (C) 2014  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: diff current spec file with cblrpm spec output

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Commands.Diff (
  diff
  ) where

import Commands.Spec (createSpecFile)
import FileUtils (mktempdir)
import PackageUtils (PackageData (..))
import Setup (RpmFlags (..))
import SysCmd ((+-+), shell)

import Distribution.Simple.Utils (die)

import System.Directory (removeDirectoryRecursive)

diff :: PackageData -> RpmFlags -> IO ()
diff pkgFiles flags =
  case specFilename pkgFiles of
    Nothing -> die "No (unique) .spec file in directory."
    Just spec -> do
      tmpdir <- mktempdir
      speccblrpm <- createSpecFile pkgFiles flags (Just tmpdir)
      shell $ "diff" +-+ "-u" +-+ spec +-+ speccblrpm +-+ "| sed -e s%" ++ speccblrpm ++ "%" ++ spec ++ ".cblrpm" ++ "%"
      removeDirectoryRecursive tmpdir
