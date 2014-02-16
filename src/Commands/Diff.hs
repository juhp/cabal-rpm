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

import Dependencies (packageDependencies, showDep)
import PackageUtils (isScmDir, packageName, packageVersion)
import Setup (RpmFlags (..))
import SysCmd ((+-+))

--import Control.Exception (bracket)
import Control.Monad    (unless, when)
import Data.Char        (toLower, toUpper)
import Data.List        (groupBy, isPrefixOf, isSuffixOf, sort)
import Data.Maybe       (fromMaybe)

import Distribution.Simple.Utils (notice, warn)

import Distribution.PackageDescription (PackageDescription (..), exeName,
                                        hasExes, hasLibs, withExe)

import System.Directory (doesFileExist, getDirectoryContents)
import System.IO     (IOMode (..), hClose, hPutStrLn, openFile)
import System.Locale (defaultTimeLocale)
import System.FilePath (dropFileName, takeDirectory)

diff ::    FilePath            -- ^arg path
        -> PackageDescription  -- ^pkg description
        -> RpmFlags            -- ^rpm flags
        -> IO ()
diff cabalPath pkgDesc flags = do
  isfile <- doesFileExist path
  let dir = if is file then takeDirectory path else path
  spcfile <- fileWithExtension "." ".spec"
  if not spcfile
    then die "No (unique) .spec file in current directory."
    else do
    cwd <- getCurrentDirectory
    tmpdir <- mktempdir
    setCurrentDirectory tmpdir
    createSpecFile cabalPath pkgDesc opts
