-- |
-- Module      :  FileUtils
-- Copyright   :  (C) 2014  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Command line option processing for building RPM
-- packages.

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module FileUtils (
  fileWithExtension,
  fileWithExtension_,
  mktempdir) where

import SysCmd (tryReadProcess)

import Control.Applicative ((<$>))
import Data.Maybe (isJust)
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension, (</>))

-- looks in current dir for a unique file with given extension
fileWithExtension :: FilePath -> String -> IO (Maybe FilePath)
fileWithExtension dir ext = do
  files <- filter (\ f -> takeExtension f == ext) <$> getDirectoryContents dir
  case files of
       [file] -> return $ Just $ dir </> file
       [] -> return Nothing
       _ -> putStrLn ("More than one " ++ ext ++ " file found!") >> return Nothing

-- looks in current dir for a unique file with given extension
fileWithExtension_ :: FilePath -> String -> IO Bool
fileWithExtension_ dir ext =
  isJust <$> fileWithExtension dir ext

mktempdir :: IO FilePath
mktempdir = do
  mktempOut <- tryReadProcess "mktemp" ["-d"]
  return $ init mktempOut
