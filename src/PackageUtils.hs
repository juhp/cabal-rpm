{-# LANGUAGE CPP #-}

-- |
-- Module      :  PackageUtils
-- Copyright   :  (C) 2013-2020  Jens Petersen
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
  checkForSpecFile,
  dropChangelog,
  editSpecField,
  getRevisedCabal,
  getSpecField,
  latestPackage,
  PackageData (..),
  packageManager,
  patchSpec,
  pkgSpecPkgData,
  prepare,
  repoquery,
  rpmbuild,
  rpmInstall,
  RpmStage (..)
  ) where

import FileUtils (assertFileNonEmpty, filesWithExtension, fileWithExtension,
                  listDirectory', withTempDirectory)
import SimpleCabal (finalPackageDescription, licenseFiles, mkPackageName,
                    PackageDescription, PackageIdentifier(..), PackageName,
                    tryFindPackageDesc)
import SimpleCmd (cmd, cmd_, cmdBool, cmdIgnoreErr, cmdLines,
#if MIN_VERSION_simple_cmd(0,2,2)
                  cmdStderrToStdout,
#else
                  cmdSilent,
#endif
                  error', grep_, removePrefix, sudo, sudo_, (+-+))
import SimpleCmd.Git (isGitDir, grepGitConfig)
import SimpleCmd.Rpm (rpmspec)
import SysCmd (optionalProgram, requireProgram, rpmEval)
#if MIN_VERSION_simple_cmd(0,2,2)
import System.Exit (ExitCode (..))
#endif
import Stackage (defaultLTS, latestStackage)
import Types

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad    (filterM, unless, void, when)

import Data.List.Extra
import Data.Maybe
import Data.Time.Clock (diffUTCTime, getCurrentTime)

import Distribution.Text (display, simpleParse)
#if MIN_VERSION_Cabal(3,6,0)
import Distribution.Utils.Path (getSymbolicPath)
#endif

import System.Directory
import System.Environment (getEnv)
import System.FilePath
import System.IO (hIsTerminalDevice, stdout)
import System.Posix.Files (accessTime, fileMode, getFileStatus,
                           modificationTime, setFileMode)

simplePackageDescription :: Flags -> FilePath
                         -> IO (PackageDescription, [FilePath], [FilePath])
simplePackageDescription flags cabalfile = do
  final <- finalPackageDescription flags cabalfile
  (docs, licensefiles) <- findDocsLicenses (dropFileName cabalfile) final
  return (final, docs, licensefiles)

-- FIXME only include (doc) files listed in the .cabal file
-- eg ChangeLog.md may exist but not dist packaged
findDocsLicenses :: FilePath -> PackageDescription -> IO ([FilePath], [FilePath])
findDocsLicenses dir pkgDesc = do
  contents <- listDirectory' dir
  let docs = sort $ filter unlikely $ filter (likely docNames) contents
  let licenses = sort $ nub
                 (map getSymbolicPath (licenseFiles pkgDesc)
                  ++ filter (likely licenseNames) contents)
      docfiles = if null licenses then docs else filter (`notElem` licenses) docs
  return (docfiles, licenses)
  where docNames = ["announce", "author", "bugs", "changelog", "changes",
                    "contribut", "example", "news", "readme", "todo"]
        licenseNames = ["copying", "licence", "license"]
        likely :: [String] -> String -> Bool
        likely names name = any (`isPrefixOf` lower name) names
        unlikely name = not $ any (`isSuffixOf` name) ["~", ".cabal"]

bringTarball :: PackageIdentifier -> Maybe FilePath -> IO ()
bringTarball pkgid mspec = do
  let tarfile = display pkgid <.> "tar.gz"
  havespec <- case mspec of
                Nothing -> return False
                Just spec -> doesFileExist spec
  sources <- if havespec
             then map sourceFieldFile <$> cmdLines "spectool" ["-S", fromJust mspec]
             else return [tarfile]
  srcdir <- getSourceDir
  allExist <- and <$> mapM (doesFileExist . (srcdir </>)) sources
  unless allExist $ do
    pkggit <- grepGitConfig "\\(pkgs\\|src\\)."
    when pkggit $ do
      srcnv <- grep_ tarfile "sources"
      when srcnv $
        cmd_ "fedpkg" ["sources"]
    when havespec $
      createDirectoryIfMissing True srcdir
    mapM_ (copyTarball False srcdir) sources
    haveLocalCabal <- doesFileExist $ srcdir </> display pkgid <.> "cabal"
    unless haveLocalCabal $
      void $ getRevisedCabal pkgid
    allExist' <- and <$> mapM (doesFileExist . (srcdir </>)) sources
    when (not allExist' && havespec) $
      cmd_ "spectool" ["-g", "-S", "-C", srcdir, fromJust mspec]
 where
  sourceFieldFile :: String -> FilePath
  sourceFieldFile field =
    if null field then
      -- should be impossible
      error "empty source field!"
    else (takeFileName . last . words) field

  copyTarball :: Bool -> FilePath -> FilePath -> IO ()
  copyTarball ranFetch dir file =
   when (takeExtensions file == ".tar.gz") $ do
    let dest = dir </> file
    already <- doesFileExist dest
    unless already $ do
      home <- getEnv "HOME"
      let cacheparent = home </> ".cabal" </> "packages"
      havecache <- doesDirectoryExist cacheparent
      unless havecache cabalUpdate
      remotes <- listDirectory' cacheparent

      let pkgid' =
            fromMaybe (error' $ "Parse failed for:" +-+ dropExtensions file) $
            simpleParse (dropExtensions file)
          tarfile = display pkgid' <.> "tar.gz"
          tarpath =
              display (pkgName pkgid') </> display (pkgVersion pkgid') </> tarfile
          paths = map (\ repo -> cacheparent </> repo </> tarpath) remotes
      -- if more than one tarball, should maybe warn if they are different
      tarballs <- filterM doesFileExist paths
      if null tarballs
        then if ranFetch
             then error $ "no" +-+ tarfile +-+ "found"
             else do
             cabal_ "fetch" ["-v0", "--no-dependencies", display pkgid']
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
    if git
      then getCurrentDirectory
      else fromJust <$> rpmEval "%{_sourcedir}"

getBuildDir :: IO FilePath
getBuildDir =
  fromJust <$> rpmEval "%{_builddir}"

getRevisedCabal :: PackageIdentifier -> IO Bool
getRevisedCabal pkgid = do
  let file = display (pkgName pkgid) <.> "cabal"
  dir <- getSourceDir
  withTempDirectory $ do
    -- FIXME use cabal-file
    dl <- cmdBool "wget" ["--quiet", "https://hackage.haskell.org/package" </> display pkgid </> file]
    if not dl
      then return False
      else do
      revised <- grep_ "x-revision" file
      when revised $
        -- renameFile can fail across fs devices
        copyFile file $ dir </> display pkgid <.> "cabal"
      return revised

data RpmStage = Binary | Source | Prep
  deriving Eq

instance Show RpmStage where
  show Binary = "binary"
  show Source = "source"
  show Prep = "prep"

rpmbuild :: Bool -> RpmStage -> FilePath -> IO ()
rpmbuild quiet mode spec = do
  let rpmCmd = case mode of
        Binary -> "a"
        Source -> "s"
        Prep -> "p"
  cwd <- getCurrentDirectory
  gitDir <- isGitDir "."
  let rpmdirs_override =
        [ "--define="++ mcr +-+ cwd | gitDir, mcr <- ["_sourcedir"]]
  let args = ["-b" ++ rpmCmd] ++ ["--nodeps" | mode == Prep] ++
             rpmdirs_override ++ [spec]
  if not quiet then
    cmd_ "rpmbuild" args
    else do
    putStr $ "rpmbuild" +-+ show mode ++ ": "
#if MIN_VERSION_simple_cmd(0,2,2)
    -- may hang for build
    (ret, out) <- cmdStderrToStdout "rpmbuild" args
    case ret of
      ExitSuccess -> putStrLn "done"
      ExitFailure _ -> error' $ "\n" ++ dropToPrefix "+ /usr/bin/chmod -Rf" out
  where
    dropToPrefix :: String -> String -> String
    dropToPrefix _ "" = ""
    dropToPrefix prefix cs =
      let ls = lines cs
          rest = dropWhile (not . (prefix `isPrefixOf`)) ls
      in unlines (if null rest then tail ls else tail rest)
#else
    cmdSilent "rpmbuild" args
    putStrLn "done"
#endif

cabalUpdate :: IO ()
cabalUpdate = do
  home <- getEnv "HOME"
  let dir = home </> ".cabal/packages/hackage.haskell.org"
  done <- checkTimestamp $ dir </> "01-index.timestamp"
  unless done $ do
    done' <- checkTimestamp $ dir </> "01-index.cache"
    unless done' cabalUpdateCmd
  where
    checkTimestamp tsfile = do
      haveFile <- doesFileExist tsfile
      if haveFile then do
        ts <- getModificationTime tsfile
        t <- getCurrentTime
        -- less than 3 hours
        when (diffUTCTime t ts > 10000) cabalUpdateCmd
        return True
        else
        return False

    cabalUpdateCmd :: IO ()
    cabalUpdateCmd = do
      putStrLn "Running 'cabal update'"
      cmd_ "cabal" ["update", "-v0"]

cabal :: String -> [String] -> IO [String]
cabal c args = do
  cabalUpdate
  cmdLines "cabal" (c:args)

cabal_ :: String -> [String] -> IO ()
cabal_ c args = do
  cabalUpdate
  cmd_ "cabal" (c:args)


tryUnpack :: PackageIdentifier -> IO FilePath
tryUnpack pkgid = do
  builddir <- getBuildDir
  let dir = builddir </> display pkgid
  isdir <- doesDirectoryExist dir
  if isdir
    then do
    mcabal <- withCurrentDirectory dir $ checkForCabalFile (Just (pkgName pkgid))
    if isJust mcabal then do
      pth <- tryFindPackageDesc dir
      return pth
      else
      error $ "could not find" +-+ display (pkgName pkgid) <.> "cabal"
    else do
    createDirectoryIfMissing True builddir
    withCurrentDirectory builddir $ do
      cabal_ "unpack" ["-v0", display pkgid]
      tryFindPackageDesc dir

latestPackage :: Maybe Stream -> PackageName -> IO PackageIdentifier
latestPackage (Just Hackage) pkg = latestHackage pkg
latestPackage mstream pkg = do
  stk <- latestStackage mstream pkg
  case stk of
    Just pkgid -> return pkgid
    Nothing -> latestHackage pkg

-- use cabal-file when it parses preferred-versions
latestHackage :: PackageName -> IO PackageIdentifier
latestHackage pkgname = do
  let pkg = display pkgname
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
      return $ PackageIdentifier (mkPackageName pkg) $ readVersion $ head avails

checkForSpecFile :: Maybe PackageName -> IO (Maybe FilePath)
checkForSpecFile mpkg = do
  allSpecs <- filesWithExtension "." ".spec"
  -- emacs makes ".#*.spec" tmp files
  let predicate = maybe ((/= '.') . head) ((\ pkg -> (`elem` [pkg <.> "spec", "ghc-" ++ pkg <.> "spec"])) . display) mpkg
      specs = filter predicate allSpecs
  when (specs /= allSpecs && isNothing mpkg) $
    putStrLn "Warning: dir contains a hidden spec file"
  case specs of
    [one] -> return $ Just one
    [] -> return Nothing
    _ -> error' "more than one spec file found!"

checkForCabalFile :: Maybe PackageName -> IO (Maybe FilePath)
checkForCabalFile mpkg = do
  allCabals <- filesWithExtension "." ".cabal"
  let predicate = maybe (const True) ((\ pkg -> (== pkg <.> "cabal")) . display) mpkg
      cabals = filter predicate allCabals
  case cabals of
    [one] -> return $ Just one
    [] -> return Nothing
    _ -> error' "more than one cabal file found!"

checkForPkgCabalFile :: PackageIdentifier -> IO (Maybe FilePath)
checkForPkgCabalFile pkgid = do
  let pkg = pkgName pkgid
      cabalfile = display pkg <.> "cabal"
  pkgcabal <- doesFileExist cabalfile
  if pkgcabal
    then return $ Just cabalfile
    else do
    exists <- doesDirectoryExist $ display pkgid
    if exists
      then fileWithExtension (display pkgid) ".cabal"
      else return Nothing

pkgSpecPkgData :: Flags -> Maybe PackageName -> IO PackageData
pkgSpecPkgData flags mpkg = do
  mspec <- checkForSpecFile mpkg
  case mspec of
    Just spec -> specPackageData spec
    Nothing -> do
      mcabal <- checkForCabalFile mpkg
      case mcabal of
        Just cabalfile -> do
          (pkgDesc, docs, licenses) <- simplePackageDescription flags cabalfile
          return $ PackageData Nothing docs licenses pkgDesc
        Nothing ->
          case mpkg of
            Just pkg -> prepStreamPkg flags Nothing defaultLTS pkg
            Nothing -> do
              cwd <- getCurrentDirectory
              case simpleParse (takeFileName cwd) of
                Just pdir ->
                  prepare flags (streamPkgToPVS Nothing (Just pdir))
                Nothing -> error' "package not found for directory"
  where
    specPackageData :: FilePath -> IO PackageData
    specPackageData spec = do
      assertFileNonEmpty spec
      cabalfile <- cabalFromSpec spec
      (pkgDesc, docs, licenses) <- simplePackageDescription flags cabalfile
      return $ PackageData (Just spec) docs licenses pkgDesc
      where
        cabalFromSpec :: FilePath -> IO FilePath
        cabalFromSpec specFile = do
          namever <- removePrefix "ghc-" . head <$> rpmspec ["--srpm"] (Just "%{name}-%{version}") specFile
          case simpleParse namever of
            Nothing -> error "pkgid could not be parsed"
            Just pkgid -> bringTarball pkgid (Just specFile)
          builddir <- getBuildDir
          let pkgsrcdir = builddir </> namever
          dExists <- doesDirectoryExist pkgsrcdir
          if dExists
            then do
            specTime <- modificationTime <$> getFileStatus specFile
            dirTime <- accessTime <$> getFileStatus pkgsrcdir
            when (specTime > dirTime) $ do
              rpmbuild True Prep specFile
              dExists' <- doesDirectoryExist pkgsrcdir
              when dExists' $ cmd_ "touch" [pkgsrcdir]
            else
            rpmbuild True Prep specFile
          tryFindPackageDesc pkgsrcdir

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

prepPkgId :: Flags -> Maybe FilePath -> PackageIdentifier -> IO PackageData
prepPkgId flags mspec pkgid = do
  void $ getRevisedCabal pkgid
  cabalfile <- tryUnpack pkgid
  (pkgDesc, docs, licenses) <- simplePackageDescription flags cabalfile
  return $ PackageData mspec docs licenses pkgDesc

prepStreamPkg :: Flags -> Maybe FilePath -> Stream -> PackageName
              -> IO PackageData
prepStreamPkg flags mspec stream pkg = do
  pkgid <- latestPackage (Just stream) pkg
  mcabal <- checkForPkgCabalFile pkgid
  case mcabal of
    Just cabalfile -> do
      (pkgDesc, docs, licenses) <- simplePackageDescription flags cabalfile
      return $ PackageData mspec docs licenses pkgDesc
    Nothing -> prepPkgId flags mspec pkgid

-- Nothing means package in cwd
prepare :: Flags -> Maybe PackageVersionSpecifier -> IO PackageData
prepare flags Nothing = pkgSpecPkgData flags Nothing
-- Something implies either new packaging or some existing spec file in dir
prepare flags (Just pvs) =
  case pvs of
    PVPackageName pkg -> pkgSpecPkgData flags (Just pkg)
    PVPackageId pkgid -> do
      mspec <- checkForSpecFile $ Just (pkgName pkgid)
      mcabal <- checkForPkgCabalFile pkgid
      case mcabal of
        Just cabalfile -> do
          (pkgDesc, docs, licenses) <- simplePackageDescription flags cabalfile
          return $ PackageData mspec docs licenses pkgDesc
        Nothing -> prepPkgId flags mspec pkgid
    PVStreamPackage stream Nothing -> do
      mspec <- checkForSpecFile Nothing
      case mspec of
        Nothing -> do
          cwd <- getCurrentDirectory
          let trydir = simpleParse (takeFileName cwd)
          case trydir of
            Just pdir | pkgVersion pdir == nullVersion ->
                          prepare flags (streamPkgToPVS (Just stream) trydir)
            _ -> error' "package not found"
        Just spec -> do
          let pkg = mkPackageName $ removePrefix "ghc-" $ takeBaseName spec
          prepStreamPkg flags (Just spec) stream pkg
    PVStreamPackage stream (Just pkg) -> do
      mspec <- checkForSpecFile (Just pkg)
      prepStreamPkg flags mspec stream pkg

-- redundant mdir was earlier for update
patchSpec :: Bool -> Maybe FilePath -> FilePath -> FilePath -> IO ()
patchSpec dryrun mdir oldspec newspec = do
  diff <- cmdIgnoreErr "diff" ["-u2", "-I", "- spec file generated by cabal-rpm", "-I", "Fedora Haskell SIG <haskell@lists.fedoraproject.org>", oldspec, newspec] "" >>= cmdIgnoreErr "sed" ["-e", "s%.Cblrpm/%%", "-e", "s/.cblrpm//"] . dropChangelog
  putStrLn diff
  unless dryrun $ do
    putStrLn ""
    out <- cmdIgnoreErr "patch" opts diff
    putStrLn out
  where
    opts = ["--fuzz=1"] ++ ["-p" ++ show n | let n = count '/' (removePrefix ".Cblrpm/" newspec)] ++ maybe [] (\ d -> ["-d", d]) mdir

    count :: Eq a => a -> [a] -> Int
    count x =  length . filter (==x)

dropChangelog :: String -> String
dropChangelog cs =
  let ls = lines cs in
  if " %changelog" `elem` ls then
    let rest = (dropWhileEnd ("@@ " `isPrefixOf`) . dropWhileEnd (== " ") . takeWhile (/= " %changelog")) ls in
      if length rest > 2 then unlines rest else ""
    else cs

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
  sudo prog (subcmd ++ args ++ [key])

rpmInstall :: Bool -> [String] -> IO ()
rpmInstall _ [] = return ()
rpmInstall yes rpms = do
  pkginstaller <- packageManager
  let (inst, arg) = if pkginstaller == "dnf" then ("dnf", "install") else ("yum", "localinstall")
  tty <- hIsTerminalDevice stdout
  sudo_ inst $ ["-y" | yes || not tty] ++ [arg] ++ rpms

editSpecField :: String -> String -> FilePath -> IO ()
editSpecField field new spec =
  cmd_ "sed" ["-i", "-e s/^\\(" ++ field ++ ":\\s\\+\\).*/\\1" ++ new ++ "/", spec]

getSpecField :: String -> FilePath -> IO String
getSpecField field spec =
  cmd "rpmspec" ["-q", "--qf", "%{" ++ field ++ "}", "--srpm", "--undefine", "dist", spec]

#if !MIN_VERSION_Cabal(3,6,0)
getSymbolicPath :: FilePath -> String
getSymbolicPath = id
#endif
