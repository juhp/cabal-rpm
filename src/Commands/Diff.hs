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
import FileUtils (fileWithExtension, mktempdir)
import Setup (RpmFlags (..))
import SysCmd ((+-+), runSystem)

import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
import Distribution.PackageDescription (PackageDescription (..))
import Distribution.Simple.Utils (die)

import System.Directory (removeDirectoryRecursive)

diff ::    FilePath            -- ^cabal path
        -> PackageDescription  -- ^pkg description
        -> RpmFlags            -- ^rpm flags
        -> IO ()
diff cabalPath pkgDesc flags = do
  mspcfile <- fileWithExtension "." ".spec"
  case mspcfile of
    Nothing -> die "No (unique) .spec file in directory."
    Just spec -> do
      tmpdir <- mktempdir
      createSpecFile cabalPath pkgDesc flags (Just tmpdir)
      speccblrpm <- fromJust <$> fileWithExtension tmpdir ".spec"
      runSystem $ "diff" +-+ "-u" +-+ spec +-+ speccblrpm +-+ "| sed -e s%" ++ speccblrpm ++ "%" ++ spec ++ ".cblrpm" ++ "%"
      removeDirectoryRecursive tmpdir
