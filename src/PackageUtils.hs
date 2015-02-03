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
  bringTarball,
  checkForSpecFile,
  copyTarball,
  getPkgName,
  isGitDir,
  isScmDir,
  latestPkg,
  PackageData (..),
  packageName,
  packageVersion,
  prepare,
  removePrefix,
  rpmbuild,
  RpmStage (..),
  simplePackageDescription,
  stripPkgDevel
  ) where

import FileUtils (filesWithExtension, fileWithExtension,
                  getDirectoryContents_, mktempdir)
import Setup (RpmFlags (..))
import SysCmd (cmd, cmd_, cmdSilent, (+-+))

import Control.Applicative ((<$>))
import Control.Monad    (filterM, unless, when)

import Data.Char (isDigit)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe, isJust)
import Data.Version     (showVersion)

import Distribution.Compiler (CompilerFlavor (..))
import Distribution.Package  (PackageIdentifier (..),
                              PackageName (..))
import Distribution.PackageDescription (PackageDescription (..),
                                        hasExes, hasLibs)
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)

import Distribution.Simple.Compiler (Compiler (..))
import Distribution.Simple.Configure (
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,18,0)
  configCompilerEx)
#else
  configCompiler)
#endif
import Distribution.Simple.Program   (defaultProgramConfiguration)
import Distribution.Simple.Utils (die, findPackageDesc)

import Distribution.System (Platform (..), buildArch, buildOS)

import System.Directory (copyFile, doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, setCurrentDirectory)
import System.Environment (getEnv)
import System.FilePath ((</>), (<.>), takeBaseName, takeFileName)
import System.Posix.Files (accessTime, fileMode, getFileStatus,
                           modificationTime, setFileMode)

-- returns path to .cabal file and possibly tmpdir to be removed
--findCabalFile :: Verbosity -> FilePath -> IO (FilePath, Maybe FilePath)
--findCabalFile vb path = do

stripVersion :: String -> String
stripVersion n | '-' `notElem` n = n
stripVersion nv = if hasVer then reverse mEman else nv
  where
    (mRev, '-':mEman) = break (== '-') $ reverse nv
    hasVer = all (\c -> isDigit c || c == '.') mRev

simplePackageDescription :: FilePath -> RpmFlags
                         -> IO PackageDescription
simplePackageDescription path opts = do
  let verbose = rpmVerbosity opts
  genPkgDesc <- readPackageDescription verbose path
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,18,0)
  (compiler, _, _) <- configCompilerEx
#else
  (compiler, _) <- configCompiler
#endif
                   (Just GHC) Nothing Nothing defaultProgramConfiguration verbose
  case finalizePackageDescription (rpmConfigurationsFlags opts)
       (const True) (Platform buildArch buildOS) (compilerId compiler)
       [] genPkgDesc of
    Left e -> die $ "finalize failed: " ++ show e
    Right (pd, _) -> return pd

findPackageDesc' :: FilePath -> IO FilePath
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,20,0)
findPackageDesc' pth = do
  res <- findPackageDesc pth
  case res of
    Left err -> error err
    Right cbl -> return cbl
#else
findPackageDesc' = findPackageDesc
#endif

cabalFromSpec :: FilePath -> IO (FilePath, Maybe FilePath)
cabalFromSpec specFile = do
  -- no rpmspec command in RHEL 5 and 6
  namever <- removePrefix "ghc-" . head . lines <$> cmd "rpm" ["-q", "--qf", "%{name}-%{version}\n", "--specfile", specFile]
  dExists <- doesDirectoryExist namever
  if dExists
    then do
    specTime <- modificationTime <$> getFileStatus specFile
    dirTime <- accessTime <$> getFileStatus namever
    when (specTime > dirTime) $ do
      bringTarball namever
      rpmbuild Prep True Nothing specFile
    cabal <- findPackageDesc' namever
    return (cabal, Nothing)
    else do
    tmpdir <- mktempdir
    bringTarball namever
    rpmbuild Prep True (Just tmpdir) specFile
    cabal <- findPackageDesc' $ tmpdir </> namever
    return (cabal, Just tmpdir)

bringTarball :: FilePath -> IO ()
bringTarball nv = do
  srcdir <- do
    cwd <- getCurrentDirectory
    git <- isGitDir cwd
    if git then return cwd else cmd "rpm" ["--eval", "%{_sourcedir}"]
  fExists <- doesFileExist $ srcdir </> nv <.> "tar.gz"
  unless fExists $
    let (n, v) = nameVersion nv in
    copyTarball n v False srcdir

nameVersion :: String -> (String, String)
nameVersion nv =
  if '-' `notElem` nv
    then error $ "nameVersion: malformed NAME-VER string" +-+ nv
    else (reverse eman, reverse rev)
  where
    (rev, '-':eman) = break (== '-') $ reverse nv

data RpmStage = Binary | Source | Prep | BuildDep deriving Eq

rpmbuild :: RpmStage -> Bool -> Maybe FilePath -> FilePath -> IO ()
rpmbuild mode quiet moutdir spec = do
  let rpmCmd = case mode of
        Binary -> "a"
        Source -> "s"
        Prep -> "p"
        BuildDep -> "_"
  cwd <- getCurrentDirectory
  gitDir <- isGitDir cwd
  let rpmdirs_override = if gitDir
                         then ["--define=_rpmdir" +-+ cwd,
                               "--define=_srcrpmdir" +-+ cwd,
                               "--define=_sourcedir" +-+ cwd]
                         else []
  command "rpmbuild" $ ["-b" ++ rpmCmd] ++
    ["--nodeps" | mode == Prep] ++
    ["--define=_builddir" +-+ maybe cwd (cwd </>) moutdir | isJust moutdir] ++
    rpmdirs_override ++
    [spec]
  where
    command = if quiet then cmdSilent else cmd_

removePrefix :: String -> String-> String
removePrefix pref str = fromMaybe str (stripPrefix pref str)

removeSuffix :: String -> String -> String
removeSuffix suffix orig =
  fromMaybe orig $ stripSuffix suffix orig
  where
    stripSuffix sf str = reverse <$> stripPrefix (reverse sf) (reverse str)

stripPkgDevel :: String -> String
stripPkgDevel = removeSuffix "-devel" . removePrefix "ghc-"

tryUnpack :: String -> IO (FilePath, Maybe FilePath)
tryUnpack pkg = do
  pkgver <- if '.' `elem` pkg then return pkg else latestPkg pkg
  isdir <- doesDirectoryExist pkgver
  if isdir
    then do
    pth <- findPackageDesc' pkgver
    return (pth, Nothing)
    else do
    cwd <- getCurrentDirectory
    tmpdir <- mktempdir
    setCurrentDirectory tmpdir
    cmd_ "cabal" ["unpack", "-v0", pkgver]
    pth <- findPackageDesc' pkgver
    setCurrentDirectory cwd
    return (tmpdir ++ "/" ++ pth, Just tmpdir)

latestPkg :: String -> IO String
latestPkg pkg = do
  contains_pkg <- lines <$> cmd "cabal" ["list", "--simple-output", pkg]
  let pkgs = filter ((== pkg) . takeWhile (not . (== ' '))) contains_pkg
  if null pkgs
    then error $ pkg ++ " hackage not found"
    else return $ map (\c -> if c == ' ' then '-' else c) $ last pkgs

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
  isGitDir dir <||> doesDirectoryExist (dir </> "_darcs")

isGitDir :: FilePath -> IO Bool
isGitDir dir = doesDirectoryExist (dir </> ".git")

getPkgName :: Maybe FilePath -> PackageDescription -> Bool -> IO (String, Bool)
getPkgName (Just spec) pkgDesc binary = do
  let name = packageName $ package pkgDesc
      pkgname = takeBaseName spec
      hasLib = hasLibs pkgDesc
  return $ if name == pkgname || binary then (name, hasLib) else (pkgname, False)
getPkgName Nothing pkgDesc binary = do
  let name = packageName $ package pkgDesc
      hasExec = hasExes pkgDesc
      hasLib = hasLibs pkgDesc
  return $ if binary || hasExec && not hasLib then (name, hasLib) else ("ghc-" ++ name, False)

checkForSpecFile :: Maybe String -> IO (Maybe FilePath)
checkForSpecFile Nothing = do
  specs <- filter (\ f -> head f /= '.') <$> filesWithExtension "." ".spec"
  case specs of
    [one] -> return $ Just one
    _ -> return Nothing
checkForSpecFile (Just pkg) = do
  let specname = pkg <.> "spec"
  specs <- filter (`elem` [specname, "ghc-" ++ specname]) <$> filesWithExtension "." ".spec"
  case specs of
    [one] -> return $ Just one
    _ -> return Nothing

checkForCabalFile :: String -> IO (Maybe FilePath)
checkForCabalFile pkgmver = do
  let pkg = stripVersion pkgmver
      cabalfile = pkg <.> "cabal"
  pkgcabal <- doesFileExist cabalfile
  if pkgcabal
    then return $ Just cabalfile
    else do
    exists <- doesDirectoryExist pkgmver
    if exists
      then fileWithExtension pkgmver ".cabal"
      else return Nothing

-- findSpecFile :: PackageDescription -> RpmFlags -> IO (FilePath, Bool)
-- findSpecFile pkgDesc flags = do
--   pkgname <- findPkgName pkgDesc flags
--   let specfile = pkgname <.> "spec"
--   exists <- doesFileExist specfile
--   return (specfile, exists)

copyTarball :: String -> String -> Bool -> FilePath -> IO ()
copyTarball n v ranFetch dir = do
  let tarfile = n ++ "-" ++ v <.> "tar.gz"
      dest = dir </> tarfile
  already <- doesFileExist dest
  unless already $ do
    home <- getEnv "HOME"
    let cacheparent = home </> ".cabal" </> "packages"
        tarpath = n </> v </> tarfile
    pkgsdir <- doesDirectoryExist cacheparent
    unless pkgsdir $
      error $ "Run 'cabal update' to create" +-+ cacheparent
    remotes <- getDirectoryContents_ cacheparent
    let paths = map (\ repo -> cacheparent </> repo </> tarpath) remotes
    -- if more than one tarball, should maybe warn if they are different
    tarballs <- filterM doesFileExist paths
    if null tarballs
      then if ranFetch
           then error $ "No" +-+ tarfile +-+ "found"
           else do
             cmd_ "cabal" ["fetch", "-v0", "--no-dependencies", n ++ "-" ++ v]
             copyTarball n v True dir
      else do
        copyFile (head tarballs) dest
        -- cabal fetch creates tarballs with mode 0600
        stat <- getFileStatus dest
        when (fileMode stat /= 0o100644) $
          setFileMode dest 0o0644

data PackageData =
  PackageData { specFilename :: Maybe FilePath
              , cabalFilename :: FilePath
              , packageDesc :: PackageDescription
              , workingDir :: Maybe FilePath
              }

-- Nothing implies existing packaging in cwd
-- Something implies either new packaging or could be multiple spec files in dir
prepare :: Maybe String -> RpmFlags -> IO PackageData
prepare mpkgver flags = do
  let mpkg = stripVersion <$> mpkgver
  mspec <- checkForSpecFile mpkg
  case mspec of
    Just spec -> do
      (cabal, mtmp) <- cabalFromSpec spec
      pkgDesc <- simplePackageDescription cabal flags
      return $ PackageData mspec cabal pkgDesc mtmp
    Nothing ->
      case mpkgver of
        Nothing -> do
          cwd <- getCurrentDirectory
          prepare (Just $ takeFileName cwd) flags
        Just pkgmver -> do
          mcabal <- checkForCabalFile pkgmver
          case mcabal of
            Just cabal -> do
              pkgDesc <- simplePackageDescription cabal flags
              return $ PackageData Nothing cabal pkgDesc Nothing
            Nothing -> do
              (cabal, mtmp) <- tryUnpack pkgmver
              pkgDesc <- simplePackageDescription cabal flags
              return $ PackageData Nothing cabal pkgDesc mtmp
