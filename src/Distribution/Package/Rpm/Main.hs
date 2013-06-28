-- |
-- Module      :  Distribution.Package.Rpm.Main
-- Copyright   :  Bryan O'Sullivan 2007
--                Jens Petersen 2012
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Main entry point for building RPM packages.

-- This software may be used and distributed according to the terms of
-- the GNU General Public License, incorporated herein by reference.

module Distribution.Package.Rpm.Main where

import Distribution.Compiler (CompilerFlavor (..))
import Distribution.Package.Rpm (createSpecFile, rpmBuild)
import Distribution.Package.Rpm.Setup (RpmFlags (..), parseArgs)
import Distribution.Package.Rpm.Utils (tryReadProcess, trySystem)

import Distribution.PackageDescription (GenericPackageDescription (..),
                                        PackageDescription (..))
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Compiler (Compiler (..))
import Distribution.Simple.Configure (configCompiler)
import Distribution.Simple.Program   (defaultProgramConfiguration)
import Distribution.Simple.Utils (defaultPackageDesc, die, findPackageDesc)
import Distribution.System            (Platform (..), buildArch, buildOS)
import Data.List (isSuffixOf)
import Data.Maybe (isJust)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory,
                         removeDirectoryRecursive, setCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath.Posix (takeExtension)

import Text.Regex (matchRegex, mkRegex)

main :: IO ()
main = do (opts, args) <- getArgs >>= parseArgs
          let verbosity = rpmVerbosity opts
              (cmd:args') = args
          (cabalPath, mtmp) <- if null args'
                               then do
                                 pth <- defaultPackageDesc verbosity
                                 return (pth, Nothing)
                               else findCabalFile $ head args'
          let verbose = rpmVerbosity opts
          genPkgDesc <- readPackageDescription verbose cabalPath
          pkgDesc <- simplePackageDescription genPkgDesc opts
          case cmd of
               "spec" ->  createSpecFile cabalPath pkgDesc opts
               "srpm" ->  rpmBuild cabalPath pkgDesc opts False
               "build" -> rpmBuild cabalPath pkgDesc opts True
--               "install" ->
--               "builddep" ->
--               "showdeps" ->
               c -> error $ "Unknown cmd: " ++ c
          maybe (return ()) removeDirectoryRecursive mtmp

simplePackageDescription :: GenericPackageDescription -> RpmFlags
                         -> IO PackageDescription
simplePackageDescription genPkgDesc flags = do
    (compiler, _) <- configCompiler (Just GHC) Nothing Nothing
                     defaultProgramConfiguration
                     (rpmVerbosity flags)
    case finalizePackageDescription (rpmConfigurationsFlags flags)
          (const True) (Platform buildArch buildOS) (compilerId compiler)
          [] genPkgDesc of
      Left e -> die $ "finalize failed: " ++ show e
      Right (pd, _) -> return pd

-- returns path to .cabal file and possibly tmpdir to be removed
findCabalFile :: FilePath -> IO (FilePath, Maybe FilePath)
findCabalFile path = do
  isdir <- doesDirectoryExist path
  if isdir
    then do
      putStrLn $ "Using " ++ path ++ "/"
      file <- findPackageDesc path
      return (file, Nothing)
    else do
      isfile <- doesFileExist path
      if not isfile
        then if (isJust $ matchRegex pkg_re path)
             then do
               tryUnpack path
             else error $ path ++ ": No such file or directory"
        else if takeExtension path == ".cabal"
             then return (path, Nothing)
             else if isSuffixOf ".tar.gz" path
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
  pkgver <- if elem '.' pkg then return pkg
            else do
              contains_pkg <- tryReadProcess "cabal" ["list", "--simple-output", pkg]
              let pkgs = filter ((== pkg) . fst . break (== ' ')) $ lines contains_pkg
              if (null pkgs)
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
