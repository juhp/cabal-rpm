-- |
-- Module      :  Main
-- Copyright   :  (C) 2007  Bryan O'Sullivan
--                (C) 2012-2014  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Main entry point for building RPM packages.

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Main where

import Commands.Depends (depends)
import Commands.Install (install)
import Commands.RpmBuild (rpmBuild)
import Commands.Spec (createSpecFile)
import Setup (RpmFlags (..), parseArgs)
import SysCmd (tryReadProcess, trySystem)

import Control.Monad (unless)
import Data.List (isSuffixOf)
import Data.Maybe (isJust)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Utils (defaultPackageDesc, findPackageDesc)
import Distribution.Verbosity (Verbosity, silent)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory,
                         removeDirectoryRecursive, setCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath.Posix (takeExtension)

import Text.Regex (matchRegex, mkRegex)

main :: IO ()
main = do (opts, args) <- getArgs >>= parseArgs
          let verbose = rpmVerbosity opts
              (cmd:args') = args
          (cabalPath, mtmp) <- if null args'
                               then do
                                 pth <- defaultPackageDesc verbose
                                 return (pth, Nothing)
                               else findCabalFile verbose $ head args'
          genPkgDesc <- readPackageDescription verbose cabalPath
          case cmd of
               "spec" ->  createSpecFile cabalPath genPkgDesc opts
               "srpm" ->  rpmBuild cabalPath genPkgDesc opts False
               "rpm" -> rpmBuild cabalPath genPkgDesc opts True
               "install" -> install cabalPath genPkgDesc opts
               "depends" -> depends genPkgDesc opts
--               "builddep" -> 
--               "showdeps" ->
               c -> error $ "Unknown cmd: " ++ c
          maybe (return ()) removeDirectoryRecursive mtmp

  -- where
  --   -- copied from Distribution.Simple.Configure configure
  --   depResolver = if build
  --                     then not . null . PackageIndex.lookupDependency pkgs'
  --                     else (const True)
  --       pkgs' = PackageIndex.insert internalPackage installedPackageSet
  --       pid = packageId genPkgDesc
  --       internalPackage = emptyInstalledPackageInfo {
  --               Installed.installedPackageId = InstalledPackageId $ display $ pid,
  --               Installed.sourcePackageId = pid
  --             }
  --           internalPackageSet = PackageIndex.fromList [internalPackage]

-- returns path to .cabal file and possibly tmpdir to be removed
findCabalFile :: Verbosity -> FilePath -> IO (FilePath, Maybe FilePath)
findCabalFile vb path = do
  isdir <- doesDirectoryExist path
  if isdir
    then do
      unless (vb == silent) $ putStrLn $ "Using " ++ path ++ "/"
      file <- findPackageDesc path
      return (file, Nothing)
    else do
      isfile <- doesFileExist path
      if not isfile
        then if isJust $ matchRegex pkg_re path
             then tryUnpack path
             else error $ path ++ ": No such file or directory"
        else if takeExtension path == ".cabal"
             then return (path, Nothing)
             else if ".tar.gz" `isSuffixOf` path
                  then do
                    tmpdir <- mktempdir
                    trySystem $ "tar zxf " ++ path ++ " -C " ++ tmpdir ++ " *.cabal"
                    subdir <- tryReadProcess "ls" [tmpdir]
                    file <- findPackageDesc $ tmpdir ++ "/" ++ init subdir
                    return (file, Just tmpdir)
                  else error $ path ++ ": file should be a .cabal or .tar.gz file."
  where pkg_re = mkRegex "^([A-Za-z0-9-]+)(-([0-9.]+))?$"

tryUnpack :: String -> IO (FilePath, Maybe FilePath)
tryUnpack pkg = do
  pkgver <- if '.' `elem` pkg then return pkg
            else do
              contains_pkg <- tryReadProcess "cabal" ["list", "--simple-output", pkg]
              let pkgs = filter ((== pkg) . takeWhile (not . (== ' '))) $ lines contains_pkg
              if null pkgs
                then error $ pkg ++ " hackage not found"
                else return $ map (\c -> if c == ' ' then '-' else c) $ last pkgs
  isdir <- doesDirectoryExist pkgver
  if isdir
    then do
    pth <-findPackageDesc pkgver
    return (pth, Nothing)
    else do
    cwd <- getCurrentDirectory
    tmpdir <- mktempdir
    setCurrentDirectory tmpdir
    trySystem $ "cabal unpack -v0 " ++ pkgver
    pth <- findPackageDesc pkgver
    setCurrentDirectory cwd
    return (tmpdir ++ "/" ++ pth, Just tmpdir)

mktempdir :: IO FilePath
mktempdir = do
  mktempOut <- tryReadProcess "mktemp" ["-d"]
  return $ init mktempOut
