-- |
-- Module      :  PackageUtils
-- Copyright   :  (C) 2013-2018  Jens Petersen
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
  editSpecField,
  getRevisedCabal,
  getPkgName,
  getSpecField,
  latestPackage,
  nameVersion,
  PackageData (..),
  packageManager,
  packageName,
  packageVersion,
  patchSpec,
  prepare,
  readVersion,
  removePrefix,
  removeSuffix,
  repoquery,
  rpmbuild,
  rpmInstall,
  RpmStage (..),
  stripPkgDevel
  ) where

import FileUtils (filesWithExtension, fileWithExtension,
                  getDirectoryContents_, mktempdir, withTempDirectory)
import Options (RpmFlags (..))
import SimpleCmd (cmd, cmd_, cmdIgnoreErr, cmdLines, cmdSilent, grep_, sudo,
                  (+-+))
import SimpleCmd.Git (isGitDir, grepGitConfig)
import SysCmd (die, optionalProgram, requireProgram, rpmEval)
import Stackage (latestStackage)

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad    (filterM, unless, when)

import Data.Char (isDigit, toLower)
import Data.List (groupBy, isPrefixOf, isSuffixOf, nub, sort, stripPrefix)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Version (
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
                     Version,
                     makeVersion,
#else
                     Version(..),
#endif
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
#else
                     showVersion
#endif
                    )
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
import Distribution.Version (showVersion)
#else
#endif

import Distribution.Compiler
import Distribution.Package  (PackageIdentifier (..),
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,22,0)
                                        unPackageName
#else
                                        PackageName (..)
#endif
                                       )
import Distribution.PackageDescription (PackageDescription (..),
                                        hasExes, hasLibs
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,2,0)
                                       , mkFlagAssignment
#endif
                                       )
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
import Distribution.PackageDescription.Configuration (finalizePD)
import Distribution.Types.ComponentRequestedSpec (defaultComponentRequestedSpec)
#else
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
#endif
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
#elif defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
import Distribution.PackageDescription.Parse (readGenericPackageDescription)
#else
import Distribution.PackageDescription.Parse (readPackageDescription)
#endif

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
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
import Distribution.Simple.Program   (defaultProgramDb)
#else
import Distribution.Simple.Program   (defaultProgramConfiguration)
#endif
import Distribution.Simple.Utils (
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,20,0)
    tryFindPackageDesc
#else
    findPackageDesc
#endif
    )
import Distribution.System (Platform (..), buildArch, buildOS)

import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist,
                         doesFileExist, getCurrentDirectory,
                         getDirectoryContents, removeDirectoryRecursive,
                         renameFile, setCurrentDirectory)
import System.Environment (getEnv)
import System.FilePath ((</>), (<.>), dropFileName, takeBaseName, takeFileName)
import System.Posix.Files (accessTime, fileMode, getFileStatus,
                           modificationTime, setFileMode)

-- returns path to .cabal file and possibly tmpdir to be removed
--findCabalFile :: Verbosity -> FilePath -> IO (FilePath, Maybe FilePath)
--findCabalFile vb path = do

stripVersion :: String -> String
stripVersion n | '-' `notElem` n = n
stripVersion nv = if hasVer then reverse emaN else nv
  where
    (reV, '-':emaN) = break (== '-') $ reverse nv
    hasVer = all (\c -> isDigit c || c == '.') reV

simplePackageDescription :: FilePath -> RpmFlags
                         -> IO (PackageDescription, [FilePath], [FilePath])
simplePackageDescription cabalfile opts = do
  let verbose = rpmVerbosity opts
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
#else
  let readGenericPackageDescription = readPackageDescription
      defaultProgramDb = defaultProgramConfiguration
#endif

  genPkgDesc <- readGenericPackageDescription verbose cabalfile
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
                                (Just GHC) Nothing Nothing defaultProgramDb verbose
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,22,0)
                              return (compilerInfo compiler)
#else
                              return (compilerId compiler)
#endif
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,2,0)
#else
  let mkFlagAssignment = id
#endif
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
  let finalizePackageDescription flags = finalizePD flags defaultComponentRequestedSpec
#endif
  case finalizePackageDescription (mkFlagAssignment $ rpmConfigurationsFlags opts)
       (const True) (Platform buildArch buildOS)
       compiler
       [] genPkgDesc of
    Left e -> die $ "finalize failed: " ++ show e
    Right (pd, _) -> do
      (docs, licensefiles) <- findDocsLicenses (dropFileName cabalfile) pd
      return (pd, docs, licensefiles)


findDocsLicenses :: FilePath -> PackageDescription -> IO ([FilePath], [FilePath])
findDocsLicenses dir pkgDesc = do
  contents <- getDirectoryContents dir
  let docs = sort $ filter unlikely $ filter (likely docNames) contents
  let licenses = nub $
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,20,0)
        licenseFiles pkgDesc
#else
        [licenseFile pkgDesc | licenseFile pkgDesc /= ""]
#endif
        ++ filter (likely licenseNames) contents
      docfiles = if null licenses then docs else filter (`notElem` licenses) docs
  return (docfiles, licenses)
  where docNames = ["author", "changelog", "changes", "contributors",
                    "example", "news", "readme", "todo"]
        licenseNames = ["copying", "licence", "license"]
        likely names name = let lowerName = map toLower name
                      in any (`isPrefixOf` lowerName) names
        unlikely name = not $ any (`isSuffixOf` name) ["~", ".cabal"]


#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,20,0)
#else
tryFindPackageDesc :: FilePath -> IO FilePath
tryFindPackageDesc = findPackageDesc
#endif

cabalFromSpec :: FilePath -> Bool -> IO (FilePath, Maybe FilePath)
cabalFromSpec specFile revise = do
  -- no rpmspec command in RHEL 5 and 6
  namever <- removePrefix "ghc-" . head <$> cmdLines "rpm" ["-q", "--qf", "%{name}-%{version}\n", "--specfile", specFile]
  dExists <- doesDirectoryExist namever
  if dExists
    then do
    specTime <- modificationTime <$> getFileStatus specFile
    dirTime <- accessTime <$> getFileStatus namever
    when (specTime > dirTime) $ do
      bringTarball namever revise specFile
      rpmbuild Prep True Nothing specFile
    cabalfile <- tryFindPackageDesc namever
    return (cabalfile, Nothing)
    else do
    tmpdir <- mktempdir
    bringTarball namever revise specFile
    rpmbuild Prep True (Just tmpdir) specFile
    cabalfile <- tryFindPackageDesc $ tmpdir </> namever
    return (cabalfile, Just tmpdir)

bringTarball :: FilePath -> Bool -> FilePath -> IO ()
bringTarball nv revise spec = do
  havespec <- doesFileExist spec
  sources <- if havespec
             then map sourceFieldFile <$> cmdLines "spectool" ["-S", spec]
             else return [tarfile]
  srcdir <- getSourceDir
  allExist <- and <$> mapM (doesFileExist . (srcdir </>)) sources
  unless allExist $ do
    pkggit <- grepGitConfig "\\(pkgs\\|src\\)."
    when pkggit $ do
      srcnv <- grep_ tarfile "sources"
      when srcnv $
        cmd_ "fedpkg" ["sources"]
    when havespec $ do
      createDirectoryIfMissing True srcdir
      cmd_ "spectool" ["-g", "-S", "-C", srcdir, spec]
    haveLocalCabal <- doesFileExist $ srcdir </> nv <.> "cabal"
    when (not haveLocalCabal && revise) $
      getRevisedCabal nv
    mapM_ (copyTarball False srcdir) sources
 where
  tarfile = nv <.> "tar.gz"

  sourceFieldFile :: String -> FilePath
  sourceFieldFile field =
    if null field then
      -- should be impossible
      error "Empty source field!"
    else (takeFileName . last . words) field

  copyTarball :: Bool -> FilePath -> FilePath -> IO ()
  copyTarball ranFetch dir file = do
    let dest = dir </> file
    already <- doesFileExist dest
    home <- getEnv "HOME"
    unless already $ do
      let cacheparent = home </> ".cabal" </> "packages"
      havecache <- doesDirectoryExist cacheparent
      unless havecache cabalUpdate
      remotes <- getDirectoryContents_ cacheparent

      let (n,v) = nameVersion nv
          tarpath = n </> v </> tarfile
          paths = map (\ repo -> cacheparent </> repo </> tarpath) remotes
      -- if more than one tarball, should maybe warn if they are different
      tarballs <- filterM doesFileExist paths
      if null tarballs
        then if ranFetch
             then error $ "No" +-+ tarfile +-+ "found"
             else do
             cabal_ "fetch" ["-v0", "--no-dependencies", nv]
             copyTarball True dir file
        else do
        createDirectoryIfMissing True dir
        copyFile (head tarballs) dest
        -- cabal-1.18 fetch creates tarballs with mode 0600
        stat <- getFileStatus dest
        when (fileMode stat /= 0o100644) $
          setFileMode dest 0o0644

getSourceDir :: IO FilePath
getSourceDir = do
    git <- isGitDir "."
    cwd <- getCurrentDirectory
    if git then return cwd else fromJust <$> rpmEval "%{_sourcedir}"

getRevisedCabal :: String -> IO ()
getRevisedCabal nv = do
  let (n,_) = nameVersion nv
      file = n <.> "cabal"
  dir <- getSourceDir
  withTempDirectory $ \ _ -> do
    cmd_ "wget" ["--quiet", "https://hackage.haskell.org/package" </> nv </> file]
    revised <- grep_ "x-revision" file
    when revised $ do
      cmd_ "dos2unix" ["--keepdate", file]
      renameFile file $ dir </> nv <.> "cabal"

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
  gitDir <- isGitDir "."
  let rpmdirs_override =
        [ "--define="++ mcr +-+ cwd |
          mcr <- ["_rpmdir", "_srcrpmdir", "_sourcedir"], gitDir]
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
cabalUpdate = cmd_ "cabal" ["update"]

cabal :: String -> [String] -> IO [String]
cabal c args = do
  cabalUpdate
  cmdLines "cabal" (c:args)

cabal_ :: String -> [String] -> IO ()
cabal_ c args = do
  cabalUpdate
  cmd_ "cabal" (c:args)

tryUnpack :: String -> Bool -> IO (FilePath, Maybe FilePath)
tryUnpack pkgver revise = do
  isdir <- doesDirectoryExist pkgver
  if isdir
    then do
    pth <- tryFindPackageDesc pkgver
    return (pth, Nothing)
    else do
    cwd <- getCurrentDirectory
    tmpdir <- mktempdir
    setCurrentDirectory tmpdir
    cabal_ "unpack" $ ["-v0"] ++ ["--pristine" | not revise] ++ [pkgver]
    pth <- tryFindPackageDesc pkgver
    setCurrentDirectory cwd
    return (tmpdir </> pth, Just tmpdir)

latestPackage :: String -> String -> IO String
latestPackage "hackage" pkg = latestHackage pkg
latestPackage str pkg = do
  stk <- latestStackage str pkg
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

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,22,0)
#else
unPackageName :: PackageName -> String
unPackageName (PackageName n) = n
#endif

packageName :: PackageIdentifier -> String
packageName = unPackageName . pkgName

packageVersion :: PackageIdentifier -> String
packageVersion = showVersion . pkgVersion

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

data PackageData =
  PackageData { specFilename :: Maybe FilePath
              , docFilenames :: [FilePath]
              , licenseFilenames :: [FilePath]
              , packageDesc :: PackageDescription
              }

-- Nothing implies existing packaging in cwd
-- Something implies either new packaging or some existing spec file in dir
prepare :: RpmFlags -> Maybe String -> Bool -> IO PackageData
prepare flags mpkgver revise = do
  let mpkg = stripVersion <$> mpkgver
  mspec <- checkForSpecFile mpkg
  case mspec of
    Just spec -> do
      (cabalfile, mtmp) <- cabalFromSpec spec revise
      (pkgDesc, docs, licenses) <- simplePackageDescription cabalfile flags
      maybe (return ()) removeDirectoryRecursive mtmp
      return $ PackageData mspec docs licenses pkgDesc
    Nothing ->
      case mpkgver of
        Nothing -> do
          cwd <- getCurrentDirectory
          prepare flags (Just $ takeFileName cwd) revise
        Just pkgmver -> do
          mcabal <- checkForCabalFile pkgmver
          case mcabal of
            Just cabalfile -> do
              (pkgDesc, docs, licenses) <- simplePackageDescription cabalfile flags
              return $ PackageData Nothing docs licenses pkgDesc
            Nothing -> do
              pkgver <- if stripVersion pkgmver == pkgmver
                        then latestPackage (rpmStream flags) pkgmver
                        else return pkgmver
              (cabalfile, mtmp) <- tryUnpack pkgver revise
              (pkgDesc, docs, licenses) <- simplePackageDescription cabalfile flags
              maybe (return ()) removeDirectoryRecursive mtmp
              return $ PackageData Nothing docs licenses pkgDesc

patchSpec :: Maybe FilePath -> FilePath -> FilePath -> IO ()
patchSpec mdir oldspec newspec = do
  diff <- cmdIgnoreErr "diff" ["-u2", "-I", "- spec file generated by cabal-rpm", "-I", "Fedora Haskell SIG <haskell@lists.fedoraproject.org>", oldspec, newspec] "" >>= cmdIgnoreErr "sed" ["-e", "s/.cblrpm//"]
  putStrLn diff
  out <- cmdIgnoreErr "patch" opts diff
  putStrLn out
  where
    opts = ["--fuzz=1"] ++ ["-p1" | '/' `elem` newspec] ++ maybe [] (\ d -> ["-d", d]) mdir

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

rpmInstall :: [String] -> IO ()
rpmInstall [] = return ()
rpmInstall rpms = do
  pkginstaller <- packageManager
  let (inst, arg) = if pkginstaller == "dnf" then ("dnf", "install") else ("yum", "localinstall")
  sudo inst $ ["-y", arg] ++ rpms

editSpecField :: String -> String -> FilePath -> IO ()
editSpecField field new spec =
  cmd_ "sed" ["-i", "-e s/^\\(" ++ field ++ ":\\s\\+\\).*/\\1" ++ new ++ "/", spec]

getSpecField :: String -> FilePath -> IO String
getSpecField field spec =
  cmd "sed" ["-n", "-e s/^" ++ field ++ ":\\s\\+\\(\\S\\+\\)/\\1/p", spec]

readVersion :: String -> Version
readVersion = makeVersion . parseVer
  where
    parseVer :: String -> [Int]
    parseVer cs =
      let vs = filter (/= ".") $ groupBy (\ c c' -> c /= '.' && c' /= '.') cs
      in map read vs

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
    -- from Data.Version
    makeVersion :: [Int] -> Version
    makeVersion b = Version b []
#endif
