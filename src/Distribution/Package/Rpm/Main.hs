-- |
-- Module      :  Distribution.Package.Rpm.Main
-- Copyright   :  Bryan O'Sullivan 2007
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Main entry point for building RPM packages.

-- This software may be used and distributed according to the terms of
-- the GNU General Public License, incorporated herein by reference.

module Distribution.Package.Rpm.Main where

import Distribution.Package.Rpm (createSpecFile)
import Distribution.Package.Rpm.Setup (RpmFlags (..), parseArgs)
import Distribution.Simple.Utils (defaultPackageDesc, findPackageDesc)
import Control.Monad (void)
import Data.Char (isDigit)
import Data.List (isSuffixOf)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory,
                         removeDirectoryRecursive, setCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath.Posix (takeExtension)
import System.Process (readProcess, system)

main :: IO ()
main = do (opts, args) <- getArgs >>= parseArgs
          let verbosity = rpmVerbosity opts
          (cabalPath, mtmp) <- if null args
                               then do
                                 pth <- defaultPackageDesc verbosity
                                 return (pth, Nothing)
                               else findCabalFile $ head args
          void $ createSpecFile cabalPath opts
          maybe (return ()) removeDirectoryRecursive mtmp

-- returns path to .cabal file and possibly tmpdir to be removed
findCabalFile :: FilePath -> IO (FilePath, Maybe FilePath)
findCabalFile path = do
  isdir <- doesDirectoryExist path
  if isdir
    then do
      file <- findPackageDesc path
      return (file, Nothing)
    else do
      isfile <- doesFileExist path
      if not isfile
        then if (all (\ p -> p `notElem` path) "/.")
             then do
               tryUnpack path
             else error $ path ++ ": No such file or directory"
        else if takeExtension path == ".cabal"
             then return (path, Nothing)
             else if isSuffixOf ".tar.gz" path
                  then do
                    tmpdir <- mktempdir
                    _ <- system $ "tar zxf " ++ path ++ " -C " ++ tmpdir ++ " *.cabal"
                    subdir <- readProcess "ls" [tmpdir] []
                    file <- findPackageDesc $ tmpdir ++ "/" ++ init subdir
                    return (file, Just tmpdir)
                  else error $ path ++ ": file should be a .cabal or .tar.gz file."

tryUnpack :: String -> IO (FilePath, Maybe FilePath)
tryUnpack pkg = do
  pkgver <- if isDigit $ last pkg then return pkg
            else do
              contains_pkg <- readProcess "cabal" ["list", "--simple-output", pkg] []
              let pkgs = filter ((== pkg) . fst . break (== ' ')) $ lines contains_pkg
              return $ map (\c -> if c == ' ' then '-' else c) $ last pkgs
  isdir <- doesDirectoryExist pkgver
  if isdir
    then do
    pth <-findPackageDesc pkgver
    return (pth, Nothing)
    else do
    cwd <- getCurrentDirectory
    tmpdir <- mktempdir
    setCurrentDirectory tmpdir
    _ <- system $ "cabal unpack " ++ pkgver
    pth <- findPackageDesc pkgver
    setCurrentDirectory cwd
    return (tmpdir ++ "/" ++ pth, Just tmpdir)

mktempdir :: IO FilePath
mktempdir = do
  mktempOut <- readProcess "mktemp" ["-d"] []
  return $ init mktempOut
