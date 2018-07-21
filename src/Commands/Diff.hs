-- |
-- Module      :  Commands.Diff
-- Copyright   :  (C) 2014,2017-2018  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: diff current spec file with cabal-rpm spec output

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Commands.Diff (
  diff
  ) where

import Commands.Spec (createSpecFile)
import FileUtils (mktempdir, withCurrentDirectory)
import Options (RpmFlags (..))
import PackageUtils (PackageData (..), prepare)
import SysCmd ((+-+), die, shell)

import Data.Maybe (isNothing)

import System.Directory (removeDirectoryRecursive)
import System.FilePath ((<.>))
import System.Posix.Env (getEnvDefault)

diff :: PackageData -> RpmFlags -> Maybe String -> IO ()
diff pkgdata flags mpkg =
  case specFilename pkgdata of
    Nothing -> die "No (unique) .spec file in directory."
    Just spec -> do
      tmpdir <- mktempdir
      pd <- if isNothing mpkg then return pkgdata
            else withCurrentDirectory tmpdir $ prepare flags mpkg False
      speccblrpm <- createSpecFile pd flags (Just tmpdir)
      diffcmd <- getEnvDefault "CBLRPM_DIFF" "diff -u"
      shell $ diffcmd +-+ spec +-+ speccblrpm +-+ "| sed -e s%" ++ speccblrpm ++ "%" ++ spec <.> "cblrpm" ++ "%"
      removeDirectoryRecursive tmpdir
