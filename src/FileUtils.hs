{-# LANGUAGE CPP #-}

-- |
-- Module      :  FileUtils
-- Copyright   :  (C) 2014, 2016-2020  Jens Petersen
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
  assertFileNonEmpty,
  filesWithExtension,
  fileWithExtension,
  fileWithExtension_,
  listDirectory',
  mktempdir,
  withCurrentDirectory,
  withTempDirectory) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif
import Control.Exception (bracket)
import Control.Monad (when)
import Data.List
import Data.Maybe (isJust)
import SimpleCmd (cmd, error')
import System.Directory (getCurrentDirectory, listDirectory,
                         setCurrentDirectory, removeDirectoryRecursive,
#if (defined(MIN_VERSION_directory) && MIN_VERSION_directory(1,2,3))
                         withCurrentDirectory
#endif
                         )
import System.FilePath
import System.Posix.Files (fileSize, getFileStatus)

filesWithExtension :: FilePath -> String -> IO [FilePath]
filesWithExtension dir ext =
  filter (ext `isExtensionOf`) <$> listDirectory dir

-- looks in dir for a unique file with given extension
fileWithExtension :: FilePath -> String -> IO (Maybe FilePath)
fileWithExtension dir ext = do
  files <- filesWithExtension dir ext
  case files of
       [file] -> return $ Just $ dir </> file
       [] -> return Nothing
       _ -> putStrLn ("More than one " ++ ext ++ " file found!") >> return Nothing

-- looks in current dir for a unique file with given extension
fileWithExtension_ :: FilePath -> String -> IO Bool
fileWithExtension_ dir ext =
  isJust <$> fileWithExtension dir ext

mktempdir :: IO FilePath
mktempdir = cmd "mktemp" ["-d", "--tmpdir", "cblrpm.XXXXXXXXXX"]

withTempDirectory :: (FilePath -> IO a) -> IO a
withTempDirectory run = bracket
                        mktempdir
                        removeDirectoryRecursive
                        (\ tmpdir -> do
                            cwd <- getCurrentDirectory
                            setCurrentDirectory tmpdir
                            res <- run cwd
                            setCurrentDirectory cwd
                            return res)

-- listDirectory without hidden files
listDirectory' :: FilePath -> IO [FilePath]
listDirectory' dir =
  filter (not . isPrefixOf ".") <$> listDirectory dir

#if (defined(MIN_VERSION_directory) && MIN_VERSION_directory(1,2,3))
#else
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action =
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    setCurrentDirectory dir
    action
#endif

assertFileNonEmpty :: FilePath -> IO ()
assertFileNonEmpty file = do
  size <- fileSize <$> getFileStatus file
  when (size == 0) $
    error' $ file ++ " is empty!"

#if !MIN_VERSION_filepath(1,4,2)
isExtensionOf :: String -> FilePath -> Bool
isExtensionOf ext@('.':_) = isSuffixOf ext . takeExtensions
isExtensionOf ext         = isSuffixOf ('.':ext) . takeExtensions
#endif
