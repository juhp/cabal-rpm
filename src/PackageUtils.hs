-- |
-- Module      :  PackageUtils
-- Copyright   :  (C) 2013-2014  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: functions related to Cabal dependency generation.

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module PackageUtils (
  isScmDir,
  missingPackages,
  packageName,
  packageVersion,
  simplePackageDescription
    ) where

import Dependencies (packageDependencies)
import FileUtils (fileWithExtension, fileWithExtension_,
                  getDirectoryContents_, mktempdir)
import Setup (RpmFlags (..))
import SysCmd (runSystem, tryReadProcess, systemBool, (+-+))

import Control.Applicative ((<$>))
import Control.Monad    (filterM, liftM)

import Data.Char (isAlphaNum)
import Data.List (isSuffixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Version     (showVersion)

import Distribution.Compiler (CompilerFlavor (..))
import Distribution.Package  (PackageIdentifier (..),
                              PackageName (..))
import Distribution.PackageDescription (PackageDescription (..))
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)

import Distribution.Simple.Compiler (Compiler (..))
import Distribution.Simple.Configure (configCompiler)
import Distribution.Simple.Program   (defaultProgramConfiguration)
import Distribution.Simple.Utils (die, findPackageDesc)

import Distribution.System (Platform (..), buildArch, buildOS)
import Distribution.Verbosity (Verbosity)

import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>), takeExtension)


-- returns path to .cabal file and possibly tmpdir to be removed
findCabalFile :: Verbosity -> FilePath -> IO (FilePath, Maybe FilePath)
findCabalFile vb path = do
  isdir <- doesDirectoryExist path
  if isdir
    then do
      cblfile <- fileWithExtension_ path ".cabal"
      if cblfile
        then do
          file <- findPackageDesc path
          return (file, Nothing)
        else do
          spcfile <- fileWithExtension path ".spec"
          maybe (die "Cannot determine package in dir.")
            (cabalFromSpec vb) spcfile
    else do
      isfile <- doesFileExist path
      if not isfile
        then if isPackageId path
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
                         subdir <- getDirectoryContents_ tmpdir
                         file <- findPackageDesc $ tmpdir ++ "/" ++ head subdir
                         return (file, Just tmpdir)
                       else error $ path ++ ": file should be a .cabal, .spec or .tar.gz file."
  where
    isPackageId :: String -> Bool
    isPackageId (c:cs) | isAlphaNum c =
      all (\d -> isAlphaNum  d || d `elem` "-." ) cs
    isPackageId _ = False

simplePackageDescription :: FilePath -> RpmFlags
                         -> IO (FilePath, PackageDescription, Maybe FilePath)
simplePackageDescription path opts = do
  let verbose = rpmVerbosity opts
  (cabalPath, mtmp) <- findCabalFile verbose path
  genPkgDesc <- readPackageDescription verbose cabalPath
  (compiler, _) <- configCompiler (Just GHC) Nothing Nothing
                   defaultProgramConfiguration verbose
  case finalizePackageDescription (rpmConfigurationsFlags opts)
       (const True) (Platform buildArch buildOS) (compilerId compiler)
       [] genPkgDesc of
    Left e -> die $ "finalize failed: " ++ show e
    Right (pd, _) -> return (cabalPath, pd, mtmp)

cabalFromSpec :: Verbosity -> FilePath -> IO (FilePath, Maybe FilePath)
cabalFromSpec vrb spcfile = do
  -- no rpmspec command in RHEL 5 and 6
  namever <- removePrefix "ghc-" <$> tryReadProcess "rpm" ["-q", "--qf", "%{name}-%{version}\n", "--specfile", spcfile]
  findCabalFile vrb (head $ lines namever)
  where
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

packageName :: PackageIdentifier -> String
packageName pkg = name
  where PackageName name = pkgName pkg

packageVersion :: PackageIdentifier -> String
packageVersion = showVersion . pkgVersion

(<||>) :: IO Bool -> IO Bool -> IO Bool
(<||>) f s = do
  one <- f
  if one then return True else s

isScmDir :: FilePath -> IO Bool
isScmDir dir =
  doesDirectoryExist (dir </> ".git") <||> doesDirectoryExist (dir </> "_darcs")

notInstalled :: String -> IO Bool
notInstalled br =
  liftM not $ systemBool $ "rpm -q --whatprovides" +-+ shellQuote br
  where
    shellQuote :: String -> String
    shellQuote (c:cs) = (if c `elem` "()" then (['\\', c] ++) else (c:)) (shellQuote cs)
    shellQuote "" = ""

missingPackages :: PackageDescription -> String -> IO [String]
missingPackages pkgDesc name = do
  (deps, tools, clibs, pkgcfgs, _) <- packageDependencies pkgDesc name
  filterM notInstalled $ deps ++ ["ghc-Cabal-devel", "ghc-rpm-macros"] ++ tools ++ clibs ++ pkgcfgs
