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
import FileUtils (mktempdir)
import PackageUtils (PackageData (..), prepare)
import SysCmd (die)
import Types

import SimpleCmd ((+-+), shell_)

import Distribution.Verbosity (silent)
import System.Directory (removeDirectoryRecursive)
import System.FilePath ((<.>))
import System.Posix.Env (getEnvDefault)

diff :: Flags -> PackageType -> Bool -> Stream -> Maybe Package -> IO ()
diff flags pkgtype subpackage stream mpkg = do
  pkgdata <- prepare flags stream mpkg True
  case specFilename pkgdata of
    Nothing -> die "No (unique) .spec file in directory."
    Just spec -> do
      tmpdir <- mktempdir
      speccblrpm <- createSpecFile silent flags False pkgtype subpackage stream (Just tmpdir) mpkg
      diffcmd <- getEnvDefault "CBLRPM_DIFF" "diff -u"
      shell_ $ diffcmd +-+ spec +-+ speccblrpm +-+ "| sed -e s%" ++ speccblrpm ++ "%" ++ spec <.> "cblrpm" ++ "%"
      removeDirectoryRecursive tmpdir
