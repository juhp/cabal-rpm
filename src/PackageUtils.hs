-- |
-- Module      :  PackageUtils
-- Copyright   :  (C) 2013-2017  Jens Petersen
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
  cabal_,
  copyTarball,
  getPkgName,
  isGitDir,
  isScmDir,
  latestPackage,
  nameVersion,
  PackageData (..),
  packageName,
  packageVersion,
  patchSpec,
  pkgInstall,
  prepare,
  removePrefix,
  removeSuffix,
  repoquery,
  rpmbuild,
  rpmInstall,
  RpmStage (..),
  simplePackageDescription,
  stripPkgDevel
  ) where

import FileUtils (filesWithExtension, fileWithExtension,
                  getDirectoryContents_, mktempdir)
import Options (RpmFlags (..))
import SysCmd (cmd, cmd_, cmdIgnoreErr, cmdMaybe, cmdSilent, (+-+),
               notNull, optionalProgram, requireProgram, sudo, trySystem)

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad    (filterM, unless, when)

import Data.Char (isDigit)
import Data.List (isPrefixOf, stripPrefix, (\\))
import Data.Maybe (fromMaybe, isJust, fromJust)

import Distribution.Compiler
import Distribution.Package  (PackageIdentifier (..),
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,22,0)
                                        unPackageName
#else
                                        PackageName (..)
#endif
                                       )
import Distribution.PackageDescription (PackageDescription (..),
                                        hasExes, hasLibs)
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)

import Distribution.Simple.Compiler (
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,22,0)
    compilerInfo
#else
    Compiler (..)
#endif
    )
import Distribution.Simple.Configure (
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,18,0)
    configCompilerEx
#else
    configCompiler
#endif
    )
import Distribution.Simple.Program   (defaultProgramConfiguration)
import Distribution.Simple.Utils (die
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,20,0)
    , tryFindPackageDesc
#else
    , findPackageDesc
#endif
    )

import Distribution.System (Platform (..), buildArch, buildOS)
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
import Distribution.Version (showVersion)
#else
import Data.Version (showVersion)
#endif

import System.Directory (copyFile, createDirectoryIfMissing,doesDirectoryExist,
                         doesFileExist, getCurrentDirectory,
                         setCurrentDirectory)
import System.Environment (getEnv)
import System.FilePath ((</>), (<.>), takeBaseName, takeFileName)
import System.Posix.Files (accessTime, fileMode, getFileStatus,
                           modificationTime, setFileMode)
import System.Posix.User (getEffectiveUserID)

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
  compiler <- case rpmCompilerId opts of
                Just cid -> return
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,22,0)
                              (unknownCompilerInfo cid NoAbiTag)
#else
                              cid
#endif
                Nothing -> do
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,18,0)
                              (compiler, _, _) <- configCompilerEx
#else
                              (compiler, _) <- configCompiler
#endif
                                (Just GHC) Nothing Nothing defaultProgramConfiguration verbose
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,22,0)
                              return (compilerInfo compiler)
#else
                              return (compilerId compiler)
#endif
  case finalizePackageDescription (rpmConfigurationsFlags opts)
       (const True) (Platform buildArch buildOS)
       compiler
       [] genPkgDesc of
    Left e -> die $ "finalize failed: " ++ show e
    Right (pd, _) -> return pd

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,20,0)
#else
tryFindPackageDesc :: FilePath -> IO FilePath
tryFindPackageDesc = findPackageDesc
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
    cabalfile <- tryFindPackageDesc namever
    return (cabalfile, Nothing)
    else do
    tmpdir <- mktempdir
    bringTarball namever
    rpmbuild Prep True (Just tmpdir) specFile
    cabalfile <- tryFindPackageDesc $ tmpdir </> namever
    return (cabalfile, Just tmpdir)

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

cabalUpdate :: IO ()
cabalUpdate = do
  home <- getEnv "HOME"
  let cacheparent = home </> ".cabal" </> "packages"
  pkgsdir <- doesDirectoryExist cacheparent
  unless pkgsdir $
    cmd_ "cabal" ["update"]

cabal :: String -> [String] -> IO [String]
cabal c args = do
  cabalUpdate
  lines <$> cmd "cabal" (c:args)

cabal_ :: String -> [String] -> IO ()
cabal_ c args = do
  cabalUpdate
  cmd_ "cabal" (c:args)

tryUnpack :: String -> IO (FilePath, Maybe FilePath)
tryUnpack pkg = do
  pkgver <- if stripVersion pkg == pkg then latestPackage pkg else return pkg
  isdir <- doesDirectoryExist pkgver
  if isdir
    then do
    pth <- tryFindPackageDesc pkgver
    return (pth, Nothing)
    else do
    cwd <- getCurrentDirectory
    tmpdir <- mktempdir
    setCurrentDirectory tmpdir
    cabal_ "unpack" ["-v0", pkgver]
    pth <- tryFindPackageDesc pkgver
    setCurrentDirectory cwd
    return (tmpdir </> pth, Just tmpdir)

latestPackage :: String -> IO String
latestPackage pkg = do
  stk <- latestStackage pkg
  case stk of
    Just pv -> return pv
    Nothing -> latestHackage pkg

latestHackage :: String -> IO String
latestHackage pkg = do
  contains_pkg <- cabal "list" ["-v0", pkg]
  let top = dropWhile (/= "*" +-+ pkg) contains_pkg
  if null top
    then error $ pkg +-+ "hackage not found"
    else do
    let field = "    Default available version: "
    let avails = map (removePrefix field) $ filter (isPrefixOf field) top
    if null avails
      then error $ pkg +-+ "latest available version not found"
      else do
      let res = pkg ++ "-" ++ head avails
      putStrLn $ res +-+ "in Hackage"
      return res

latestStackage :: String -> IO (Maybe String)
latestStackage pkg = do
  -- check for stackage-query
  haveStackage <- optionalProgram "stackage"
  if haveStackage
    then do
    let stream = "lts"
    mpkg <- fmap ((pkg ++ "-") ++) <$> cmdMaybe "stackage" ["package", stream, pkg]
    when (isJust mpkg) $
      putStrLn $ fromJust mpkg +-+ "in Stackage" +-+ stream
    return mpkg
    else do
    putStrLn "'cabal install stackage-query' to check against Stackage LTS"
    return Nothing

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,22,0)
#else
unPackageName :: PackageName -> String
unPackageName (PackageName n) = n
#endif

packageName :: PackageIdentifier -> String
packageName = unPackageName . pkgName

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
  -- emacs makes ".#*.spec" tmp files
  allSpecs <- allSpecfiles
  let specs = filter (\ f -> head f /= '.') allSpecs
  when (specs /= allSpecs) $
    putStrLn "Warning: dir contains a hidden spec file"
  case specs of
    [one] -> return $ Just one
    _ -> return Nothing
checkForSpecFile (Just pkg) = do
  let specname = pkg <.> "spec"
  specs <- filter (`elem` [specname, "ghc-" ++ specname]) <$> allSpecfiles
  case specs of
    [one] -> return $ Just one
    _ -> return Nothing

allSpecfiles :: IO [FilePath]
allSpecfiles = filesWithExtension "." ".spec"

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
    cabalUpdate
    home <- getEnv "HOME"
    let cacheparent = home </> ".cabal" </> "packages"
        tarpath = n </> v </> tarfile
    remotes <- getDirectoryContents_ cacheparent
    let paths = map (\ repo -> cacheparent </> repo </> tarpath) remotes
    -- if more than one tarball, should maybe warn if they are different
    tarballs <- filterM doesFileExist paths
    if null tarballs
      then if ranFetch
           then error $ "No" +-+ tarfile +-+ "found"
           else do
             cabal_ "fetch" ["-v0", "--no-dependencies", n ++ "-" ++ v]
             copyTarball n v True dir
      else do
        createDirectoryIfMissing True dir
        copyFile (head tarballs) dest
        -- cabal-1.18 fetch creates tarballs with mode 0600
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
-- Something implies either new packaging or some existing spec file in dir
prepare :: RpmFlags -> Maybe String -> IO PackageData
prepare flags mpkgver = do
  let mpkg = stripVersion <$> mpkgver
  mspec <- checkForSpecFile mpkg
  case mspec of
    Just spec -> do
      (cabalfile, mtmp) <- cabalFromSpec spec
      pkgDesc <- simplePackageDescription cabalfile flags
      return $ PackageData mspec cabalfile pkgDesc mtmp
    Nothing ->
      case mpkgver of
        Nothing -> do
          cwd <- getCurrentDirectory
          prepare flags (Just $ takeFileName cwd)
        Just pkgmver -> do
          mcabal <- checkForCabalFile pkgmver
          case mcabal of
            Just cabalfile -> do
              pkgDesc <- simplePackageDescription cabalfile flags
              return $ PackageData Nothing cabalfile pkgDesc Nothing
            Nothing -> do
              (cabalfile, mtmp) <- tryUnpack pkgmver
              pkgDesc <- simplePackageDescription cabalfile flags
              return $ PackageData Nothing cabalfile pkgDesc mtmp

patchSpec :: Maybe FilePath -> FilePath -> FilePath -> IO ()
patchSpec mdir oldspec newspec = do
  diff <- cmdIgnoreErr "diff" ["-u2", "-I", "- spec file generated by cabal-rpm", "-I", "Fedora Haskell SIG <haskell@lists.fedoraproject.org>", oldspec, newspec] "" >>= cmdIgnoreErr "sed" ["-e", "s/.cblrpm//"]
  putStrLn diff
  out <- cmdIgnoreErr "patch" opts diff
  putStrLn out
  where
    opts = maybe [] (\ d -> ["-d", d, "-p1" ]) mdir

packageManager :: IO String
packageManager = do
  havednf <- optionalProgram "dnf"
  if havednf
    then return "dnf"
    else requireProgram "yum" >> return "yum"

repoquery :: [String] -> String -> IO String
repoquery args key = do
  havednf <- optionalProgram "dnf"
  let (prog, subcmd) = if havednf then ("dnf", ["repoquery", "-q"]) else ("repoquery", [])
  cmd prog (subcmd ++ args ++ [key])

pkgInstall :: [String] -> Bool -> IO ()
pkgInstall [] _ = return ()
pkgInstall pkgs hard = do
  pkginstaller <- packageManager
  putStrLn $ "Running repoquery" +-+ unwords pkgs
  repopkgs <- filter (/= "") <$> mapM (repoquery ["--qf", "%{name}"]) pkgs
  let missing = pkgs \\ repopkgs
  if notNull missing && hard
    then error $ unwords missing +-+ "not available."
    else do
    unless (null missing) $ do
      putStrLn "Unavailable dependencies:"
      mapM_ putStrLn missing
    unless (null repopkgs) $ do
      putStrLn "Uninstalled dependencies:"
      mapM_ putStrLn repopkgs
      uid <- getEffectiveUserID
      maybeSudo <-
        if uid == 0
        then return Nothing
        else do
          havesudo <- optionalProgram "sudo"
          return $ if havesudo then Just "sudo" else Nothing
      let args = map showPkg repopkgs
      putStrLn $ "Running:" +-+ fromMaybe "" maybeSudo +-+ pkginstaller +-+ "install" +-+ unwords args
      let exec = if hard then cmd_ else trySystem
      fedora <- cmd "rpm" ["--eval", "%fedora"]
      let nogpgcheck = ["--nogpgcheck" | fedora `elem` ["22", "23"]]
      exec (fromMaybe pkginstaller maybeSudo) $ maybe [] (const [pkginstaller]) maybeSudo ++ ("install" : args ++ nogpgcheck)
        where
          showPkg :: String -> String
          showPkg p = if '(' `elem` p then show p else p

rpmInstall :: [String] -> IO ()
rpmInstall rpms = do
  pkginstaller <- packageManager
  let (inst, arg) = if pkginstaller == "dnf" then ("dnf", "install") else ("yum", "localinstall")
  sudo inst $ ["-y", arg] ++ rpms
