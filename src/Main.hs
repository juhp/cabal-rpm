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

import Commands.Depends (depends, missingDeps, requires)
import Commands.Diff (diff)
import Commands.Install (install)
import Commands.RpmBuild (rpmBuild, RpmStage (..))
import Commands.Spec (createSpecFile)

import FileUtils (fileWithExtension, fileWithExtension_, mktempdir)
import PackageUtils (simplePackageDescription)
import Setup (RpmFlags (..), parseArgs)
import SysCmd (runSystem, tryReadProcess)

import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.List (isSuffixOf, stripPrefix)
import Data.Maybe (isJust, listToMaybe, fromMaybe)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Utils (die, findPackageDesc)
import Distribution.Verbosity (Verbosity, silent)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory,
                         removeDirectoryRecursive,
                         setCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath.Posix (takeExtension)

import Text.Regex (matchRegex, mkRegex)

main :: IO ()
main = do (opts, args) <- getArgs >>= parseArgs
          let verbose = rpmVerbosity opts
              (cmd:args') = args
              path = fromMaybe "." $ listToMaybe args'
          (cabalPath, mtmp) <- findCabalFile verbose path
          genPkgDesc <- readPackageDescription verbose cabalPath
          pkgDesc <- simplePackageDescription genPkgDesc opts
          case cmd of
               "spec" ->  createSpecFile cabalPath pkgDesc opts
               "srpm" ->  rpmBuild cabalPath pkgDesc opts Source
               "prep" ->  rpmBuild cabalPath pkgDesc opts Prep
               "rpm" ->   rpmBuild cabalPath pkgDesc opts Binary
               "builddep" -> rpmBuild cabalPath pkgDesc opts BuildDep
               "install" -> install cabalPath pkgDesc
               "depends" -> depends pkgDesc
               "requires" -> requires pkgDesc
               "missingdeps" -> missingDeps pkgDesc
               "diff" -> diff path pkgDesc opts
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
      cblfile <- fileWithExtension_ path ".cabal"
      if cblfile
        then do
          unless (vb == silent) $ putStrLn $ "trying " ++ path ++ "/"
          file <- findPackageDesc path
          return (file, Nothing)
        else do
          spcfile <- fileWithExtension path ".spec"
          maybe (die "Cannot determine package in dir.")
            (cabalFromSpec vb) spcfile
    else do
      isfile <- doesFileExist path
      if not isfile
        then if isJust $ matchRegex pkg_re path
             then tryUnpack path
             else error $ path ++ ": No such file or directory"
        else if takeExtension path == ".cabal"
             then return (path, Nothing)
             else if takeExtension path == ".spec"
                  then cabalFromSpec vb path
                  else if ".tar.gz" `isSuffixOf` path
                       then do
                         tmpdir <- mktempdir
                         runSystem $ "tar zxf " ++ path ++ " -C " ++ tmpdir ++ " *.cabal"
                         subdir <- tryReadProcess "ls" [tmpdir]
                         file <- findPackageDesc $ tmpdir ++ "/" ++ init subdir
                         return (file, Just tmpdir)
                       else error $ path ++ ": file should be a .cabal, .spec or .tar.gz file."
  where pkg_re = mkRegex "^([A-Za-z0-9-]+)(-([0-9.]+))?$"

cabalFromSpec :: Verbosity -> FilePath -> IO (FilePath, Maybe FilePath)
cabalFromSpec vrb spcfile = do
  -- no rpmspec command in RHEL 5 and 6
  namever <- removePrefix "ghc-" <$> tryReadProcess "rpm" ["-q", "--qf", "%{name}-%{version}\n", "--specfile", spcfile]
  findCabalFile vrb (head $ lines namever)

removePrefix :: String -> String-> String
removePrefix pref str =
  fromMaybe str (stripPrefix pref str)

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
    runSystem $ "cabal unpack -v0 " ++ pkgver
    pth <- findPackageDesc pkgver
    setCurrentDirectory cwd
    return (tmpdir ++ "/" ++ pth, Just tmpdir)
