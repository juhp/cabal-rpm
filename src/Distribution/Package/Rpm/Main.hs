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

import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Package.Rpm (rpm)
import Distribution.Package.Rpm.Setup (RpmFlags (..), parseArgs)
import Distribution.Simple.Utils (defaultPackageDesc, findPackageDesc)
import Control.Monad (unless)
import Data.Char (isDigit)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)
import System.FilePath.Posix (takeExtension)
import System.Process (readProcess, system)

main :: IO ()
main = do (opts, args) <- getArgs >>= parseArgs
          let verbosity = rpmVerbosity opts
          descPath <- if null args then defaultPackageDesc verbosity else findCabalFile  $ head args
          pkgDesc <- readPackageDescription verbosity descPath
          rpm pkgDesc opts

findCabalFile :: FilePath -> IO FilePath
findCabalFile path = do
  isdir <- doesDirectoryExist path
  if isdir
    then findPackageDesc path
    else do
      isfile <- doesFileExist path
      if not isfile
        then if '/' `notElem` path
             then tryUnpack path
             else error $ path ++ ": No such file or directory"
        else if takeExtension path /= ".cabal"
             then error $ path ++ ": file should have .cabal extension file."
             else return path

tryUnpack :: String -> IO FilePath
tryUnpack pkg = do
  pkgver <- if isDigit $ last pkg then return pkg
            else do
              contains_pkg <- readProcess "cabal" ["list", "--simple-output", pkg] []
              let pkgs = filter (startsWith pkg) $ lines contains_pkg
              return $ map (\c -> if c == ' ' then '-' else c) $ last pkgs
  isdir <- doesDirectoryExist pkgver
  unless isdir $ do
    _ <- system $ "cabal unpack " ++ pkgver
    return ()
  findPackageDesc pkgver

startsWith :: String -> String -> Bool
startsWith pkg mth = take (length pkg') mth == pkg'
  where
    pkg' = pkg ++ " "
