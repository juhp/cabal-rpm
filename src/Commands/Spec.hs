{-# LANGUAGE CPP #-}

-- |
-- Module      :  Commands.Spec
-- Copyright   :  (C) 2007-2008  Bryan O'Sullivan
--                (C) 2012-2020  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Generates an RPM spec file from a .cabal file.

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Commands.Spec (
  createSpecFile, createSpecFile_
  ) where

import Dependencies (missingLibraries,
                     missingOtherPkgs, notSrcOrInst, packageDeps,
                     PackageDependencies(..), packageDependencies, pkgSuffix,
                     prettyShow,
                     recurseMissing, subPackages, testsuiteDependencies')
import Header (headerOption, withSpecHead)
import PackageUtils (dependencySortCabals, PackageData (..), prepare,
                     packageMacro)
import SimpleCabal (buildable, mkPackageName, PackageDescription (..),
                    PackageIdentifier(..))
import SimpleCmd ((+-+), cmd, cmdLines, cmdMaybe, grep, grep_, removePrefix)
import Types

import Control.Monad.Extra

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Data.Char        (toUpper)
import Data.List.Extra
import Data.Maybe       (isJust, isNothing, fromMaybe, fromJust)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Time.Clock  (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Version as V

import Distribution.PackageDescription (
                                        Executable (buildInfo),
                                        Library (exposedModules), exeName,
                                        hasExes, hasLibs,
#if MIN_VERSION_Cabal(2,2,0)
                                        license,
#endif
                                       )

import Distribution.Simple.Utils (warn)
import Distribution.Text (display)

#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
#endif
import Distribution.Verbosity (Verbosity)

import System.Directory (doesFileExist)
import System.IO     (IOMode (..), hClose, hPutStrLn, openFile)
import System.FilePath (takeBaseName, (</>), (<.>))

import qualified Paths_cabal_rpm (version)

#if MIN_VERSION_Cabal(3,2,0)
import qualified Distribution.Utils.ShortText as ST (fromShortText)
#endif

import qualified HTMLEntities.Decoder as HD

#if !MIN_VERSION_Cabal(2,0,0)
unUnqualComponentName :: String -> String
unUnqualComponentName = id
#endif

-- FIXME use datatype for options
createSpecFile :: Bool -> Verbosity -> Flags -> Bool -> Bool -> Bool
               -> PackageType -> Maybe (Maybe Stream) -> Maybe V.Version
               -> Maybe FilePath -> Maybe PackageVersionSpecifier
               -> IO FilePath
createSpecFile ignoreMissing verbose flags norevision notestsuite force pkgtype subpkgStream mwithghc mdest mpvs = do
  pkgdata <- prepare flags Nothing mpvs
  let mspec = case pkgtype of
                SpecFile f -> Just f
                _ -> specFilename pkgdata
      docs = docFilenames pkgdata
      licensefiles = licenseFilenames pkgdata
      pkgDesc = packageDesc pkgdata
      pkgid = package pkgDesc
      name = display $ pkgName pkgid

  mspecExists <-
    case mspec of
      Nothing -> return Nothing
      Just spec -> do
        exists <- doesFileExist spec
        return $
          if exists
          then Just spec
          else Nothing

  standalone <-
    if pkgtype == StandalonePkg then return True
    else
      ifJust mspecExists $ \spec ->
      withSpecHead spec $ return . ("--standalone" `elem`)

  subpackage <-
    if isJust subpkgStream
    then
      if pkgtype == StandalonePkg
      then do
        warn verbose "ignoring --subpackage for --standalone"
        return False
      else return True
    else
      ifJust mspecExists $ \spec ->
      withSpecHead spec $ return . ("--subpackage" `elem`)

  autorelease <-
    ifJust mspecExists $
    grep_ "^Release:        %autorelease"
  autochangelog <-
    ifJust mspecExists $
    grep_ "^%autochangelog"
  autopatch <-
    ifJust mspecExists $ \sf -> do
    patches <- not . null <$> cmdLines "spectool" ["-P", sf]
    if patches
      then do
      ppatch <- grep_ "^%patch" sf
      return $ not ppatch
      else return False

  let hasExec = hasExes pkgDesc
      hasLib = hasLibs pkgDesc
      hasLibPkg = hasLib && not standalone
      dupdocs = docs `intersect` dataFiles pkgDesc
      datafiles = dataFiles pkgDesc \\ dupdocs
      ghc_name = if isJust mwithghc then "%{ghc_name}" else "ghc"
      majorVer = V.showVersion . V.makeVersion . take 2 . V.versionBranch
      ghcname = "ghc" ++ maybe "" majorVer mwithghc
  -- FIXME is binlib misnamed?
  (pkgname, binlib) <- getPkgName mspec ghcname pkgDesc (pkgtype == BinaryPkg || standalone)
  let pkg_name = if pkgname == name then "%{name}" else "%{pkg_name}"
      basename | binlib = "%{pkg_name}"
               | hasExecPkg = name
               | otherwise = ghc_name ++ "-%{pkg_name}"
      targetSpecFile = fromMaybe "" mdest </> pkgname <.> "spec"
      hasExecPkg = binlib || (hasExec && not hasLibPkg)
  targetSpecAlreadyExists <- doesFileExist targetSpecFile
  -- run commands before opening file to prevent empty file on error
  -- maybe shell commands should be in a monad or something
  let topSpecFile = pkgname <.> "spec"
  topSpecExists <- doesFileExist topSpecFile
  droppedDeps <- if topSpecExists then
      map (mkPackageName . removePrefix "cabal-tweak-drop-dep ") <$> grep "^cabal-tweak-drop-dep " topSpecFile
      else return []
  pkgdeps <- do
    alldeps <- packageDependencies pkgDesc
    return $ alldeps {buildDeps = buildDeps alldeps \\ droppedDeps}
  let outputFile = targetSpecFile ++ if not force && targetSpecAlreadyExists then ".cblrpm" else ""
  if targetSpecAlreadyExists
    then warn verbose $ (if force then "overwriting" else "creating") +-+ outputFile
    else do
    -- changed to not
    let realdir = not . ("cblrpm." `isPrefixOf`) . takeBaseName
    when (maybe True realdir mdest) $
      putStrLn pkgname

  mstream <- case subpkgStream of
               Just (Just stream) -> return $ Just stream
               _ ->
                 case pvsStream =<< mpvs of
                   Just s -> return $ Just s
                   Nothing ->
                     case mpvs of
                       Just (PVPackageId _) -> return Nothing
                       _ -> case mspec of
                              Just spec ->
                                withSpecHead spec (return . fmap read . headerOption "--stream")
                              Nothing ->  return Nothing

  h <- openFile outputFile WriteMode
  let putHdr hdr val = hPutStrLn h (hdr ++ ":" ++ padding hdr ++ val)
      padding hdr = replicate (14 - length hdr) ' ' ++ " "
      putHdrComment hdr val = putHdr ('#':hdr) (' ':quoteMacros val)
      putNewline = hPutStrLn h ""
      sectionNewline = putNewline >> putNewline
      put = hPutStrLn h
      global m v = put $ "%global" +-+ m +-+ v
      subpkgParam lt = if binlib then "-n" +-+ ghc_name ++ "-%{name}" ++ pkgSuffix lt else show lt

  put $ "# generated by cabal-rpm-" ++ V.showVersion Paths_cabal_rpm.version
    +-+ unwords (["--standalone" | standalone] ++ ["--stream " ++ showStream (fromJust mstream) | isJust mstream, mstream > Just LatestLTS] ++ ["--subpackage" | subpackage])
  put "# https://docs.fedoraproject.org/en-US/packaging-guidelines/Haskell/"
  putNewline

  whenJust mwithghc $ \withghc -> do
    global "ghc_major" $ majorVer withghc
    global "ghc_name" "ghc%{ghc_major}"
    putNewline

  -- Some packages conflate the synopsis and description fields.  Ugh.
  let syn =
#if MIN_VERSION_Cabal(3,2,0)
        ST.fromShortText $
#endif
        synopsis pkgDesc
  when (null syn) $ warn verbose "this package has no synopsis."
  let initialCapital (c:cs) = toUpper c:cs
      initialCapital [] = []
      syn' = if null syn
             then "Haskell" +-+ name +-+ "package"
             else (unwords . lines . initialCapital) syn
      summary = dropWhileEnd (== '.') syn'
  when (length ("Summary     : " ++ syn') > 79) $
    warn verbose "this package has a long synopsis."

  let descr =
#if MIN_VERSION_Cabal(3,2,0)
        ST.fromShortText $
#endif
        description pkgDesc
  when (null descr) $ warn verbose "this package has no description."
  let descLines = (formatParagraphs . initialCapital . filterSymbols . finalPeriod . paragraphPeriods . htmlEntityDecode) $ if null descr then syn' else descr
      paragraphPeriods =
        unlines . map (\l -> if l == "." then "" else l) . lines
      finalPeriod cs =
        case last cs of
          '.' -> cs
          '\n' -> finalPeriod $ init cs
          _ -> cs ++ "."
      filterSymbols [] = []
      filterSymbols (c:cs) =
        case c of
          '@' -> '\'' : filterSymbols cs
          '\\' -> case cs of
                    (c':cs') -> c': filterSymbols cs'
                    "" -> ""
          _ -> c : filterSymbols cs

  when standalone $ do
    global "ghc_without_dynamic" "1"
    global "ghc_without_shared" "1"
    put "%undefine with_ghc_prof"
    put "%undefine with_haddock"
    global "without_prof" "1"
    global "without_haddock" "1"
    global "debug_package" "%{nil}"
    putNewline

  when hasLib $ do
    global "pkg_name" name
    global "pkgver" "%{pkg_name}-%{version}"
  when (hasLib || subpackage) $ do
    put "%{?haskell_setup}"
    putNewline

  let pkgver = if hasLib then "%{pkgver}" else pkg_name ++ "-%{version}"

  missingLibsLicenses <- do
    subs <- if subpackage then subPackages mspec pkgDesc else return []
    miss <- if subpackage || standalone
            then missingLibraries pkgDesc
            else return []
    deep <- recurseMissing flags mstream [] $ nub (subs ++ miss)
    --print deep
    return $ filter (\(d,_) -> pkgName d `notElem` droppedDeps) deep
  let version = display $ pkgVersion pkgid
  subpkgsOrdered <- dependencySortCabals (pkgname ++ '-' : version) mspec $ map fst missingLibsLicenses
  --print subpkgs
  let subpkgs = sort subpkgsOrdered
      mkSubpkgMacro = packageMacro . display . pkgName
      subpkgMacro pid = "%{" ++ mkSubpkgMacro pid ++ "}"
      subpkgMacros = map subpkgMacro subpkgs
      subpkgMacrosOrdered = map subpkgMacro subpkgsOrdered
      hasSubpkgs = not standalone &&  subpkgs /= []
  when hasSubpkgs $ do
    forM_ (sort subpkgs) $ \pid ->
      global (mkSubpkgMacro pid) $ display pid
    putNewline
    global "subpkgs" $ unwords subpkgMacrosOrdered
    putNewline

  let (testsuiteDeps,testsuiteTools) = testsuiteDependencies' pkgDesc
  unpkgedTestDeps <- filterM (notSrcOrInst . RpmHsLib Devel) testsuiteDeps
  let missTestDeps = unpkgedTestDeps \\ map pkgName subpkgs
  let testable = notNull testsuiteDeps && not standalone && null missTestDeps && not notestsuite && isNothing mwithghc
  if testable
    then do
    put "%bcond tests 1"
    putNewline
    else unless (null missTestDeps || standalone) $ do
         put $ "# testsuite missing deps: " ++ unwords (map display missTestDeps)
         putNewline
  let release = "1"
  revised <-
    if norevision
    then return False
    else
    if isJust $ lookup "x-revision" (customFieldsPD pkgDesc)
    then return True
    else do
    let local = display pkgid <.> "cabal"
    have <- doesFileExist local
    if have then grep_ "x-revision" local else return False
  revisedDOS <-
    if revised
    then do
      filetype <- cmd "file" ["-b", display pkgid <.> "cabal"]
      return $ "CRLF" `isInfixOf` filetype
    else return False

  putHdr "Name" $ if binlib then "%{pkg_name}" else basename
  putHdr "Version" version
  if autorelease && not subpackage
  then putHdr "Release" "%autorelease"
  else do
    when hasSubpkgs $
      let multipkg = if length subpkgs > 1 then "all subpkgs" else "subpkg"
      in put $ "# can only be reset when" +-+ multipkg +-+ "bumped"
    putHdr "Release" $ release ++ "%{?dist}"
  putHdr "Summary" summary
  putNewline
  putHdr "License" $ (prettyShow . license) pkgDesc
  putHdr "Url" $ "https://hackage.haskell.org/package" </> name
  put "# Begin cabal-rpm sources:"
  putHdr "Source0" $ sourceUrl pkgver
  when hasSubpkgs $
    mapM_ (\(n,p) -> putHdr ("Source" ++ n) (sourceUrl p)) $ number subpkgMacros
  when revised $
    putHdr ("Source" ++ show (1 + if hasSubpkgs then length subpkgs else 0)) $ "https://hackage.haskell.org/package" </> pkgver </> pkg_name <.> "cabal" ++ "#" </> pkgver <.> "cabal"
  put "# End cabal-rpm sources"
  putNewline
  put "# Begin cabal-rpm deps:"
  when revisedDOS $ putHdr "BuildRequires" "dos2unix"
  when (mkPackageName "Cabal" `notElem` buildDeps pkgdeps || not hasLib || notNull (setupDeps pkgdeps)) $ do
--    put "# Setup"
    unless (mkPackageName "Cabal" `elem` buildDeps pkgdeps) $
      putHdr "BuildRequires" $ ghc_name ++ "-Cabal-devel"
    mapM_ (\d -> (if d `elem` map pkgName subpkgs then putHdrComment else putHdr) "BuildRequires" (showRpm (RpmHsLib Devel d))) $ setupDeps pkgdeps
  putHdr "BuildRequires" $ "ghc-rpm-macros" ++ (if hasSubpkgs then "-extra" else "")

  unless (null (buildDeps pkgdeps)) $ do
--    put "# Build"
    when (isJust mwithghc) $ do
      put "%if %{defined ghc_name}"
      putHdr "BuildRequires" $ "%{ghc_name}" ++ "-devel"
      unless standalone $
        putHdr "BuildRequires" $ "%{ghc_name}" ++ "-prof"
      put "%else"

    let metaPackages = [mkPackageName "haskell-gi-overloading"]
    mapM_ (\d -> (if d `elem` map pkgName subpkgs then putHdrComment else putHdr) "BuildRequires" (showRpm (RpmHsLib Devel d))) $ sort $ buildDeps pkgdeps
    -- FIXME should tracks devel/prof deps finely when subpackaging
    when (hasLibPkg || hasSubpkgs) $ do
      put "%if %{with ghc_prof}"
      mapM_ (\d -> (if d `elem` map pkgName subpkgs then putHdrComment else putHdr) "BuildRequires" (showRpm (RpmHsLib Prof d))) $ sort $ buildDeps pkgdeps \\ metaPackages
      put "%endif"
  let otherdeps = sort $ toolDeps pkgdeps ++ clibDeps pkgdeps ++ pkgcfgDeps pkgdeps
  unless (null otherdeps) $ do
--    put "# Other"
    missingOthers <- missingOtherPkgs pkgDesc
    mapM_ (\d -> (if d `elem` missingOthers then putHdrComment else putHdr) "BuildRequires" d) otherdeps
  let testDeps = testsuiteDeps \\ (mkPackageName "Cabal" : (buildDeps pkgdeps ++ setupDeps pkgdeps))
  when (testable && notNull testDeps) $ do
    put "%if %{with tests}"
    mapM_ (putHdr "BuildRequires") $ (sort . map showRpm) (map (RpmHsLib Devel) testDeps ++ map RpmOther (testsuiteTools \\ toolDeps pkgdeps))
    put "%endif"

  when (isJust mwithghc) $
    put "%endif"
  let usesOptparse = hasExecPkg && any ((`elem` buildDeps pkgdeps) . mkPackageName) ["optparse-applicative","optparse-simple","simple-cmd-args"]
  when usesOptparse $
    putHdr "BuildRequires" "help2man"
  let common = binlib && datafiles /= [] && not standalone
      versionRelease = " = %{version}-%{release}"
  when common $
    putHdr "Requires" $ "%{name}-common" ++ versionRelease
  when (standalone || subpackage) $ do
    when standalone $
      putHdr "BuildRequires" "cabal-install"
    unless (null subpkgs || ignoreMissing) $ do
      -- let deptype = if standalone then Devel else Prof
      when (isJust mwithghc) $
        put "%if %{undefined ghc_name}"
      forM_ subpkgs $ \pid -> do
        let pkg = pkgName pid
        more <- packageDeps flags pid
        let moredeps = sort $ more \\ (buildDeps pkgdeps ++ map pkgName subpkgs)
        unless (null moredeps) $ do
          put $ "# for missing dep '" ++ display pkg ++ "':"
          missingMore <- filterM (notSrcOrInst . RpmHsLib Devel) moredeps
          mapM_ (\d -> (if d `elem` missingMore then putHdrComment else putHdr) "BuildRequires" (showRpm (RpmHsLib Devel d))) moredeps
          unless standalone $ do
            put "%if %{with ghc_prof}"
            mapM_ (\d -> (if d `elem` missingMore then putHdrComment else putHdr) "BuildRequires" (showRpm (RpmHsLib Prof d))) moredeps
            put "%endif"
      when (isJust mwithghc) $
        put "%endif"
  put "# End cabal-rpm deps"
  putNewline

  put "%description"
  mapM_ put descLines
  putNewline

  let wrapGenDesc = init . wordwrap (79 - max 0 (length pkgname - length pkg_name))

  when common $ do
    put "%package common"
    putHdr "Summary" $ pkg_name +-+ "common files"
    putHdr "BuildArch" "noarch"
    putNewline
    put "%description common"
    put $ wrapGenDesc $ "This package provides the" +-+ pkg_name +-+ "common data files."
    sectionNewline

  -- haskell-gi generates lib source files at configure time
  -- (strictly should also check for otherModules (autogenModules?)
  --  but libraries wihout exposedModules should be useless/redundant)
  let hasModules =
        hasLib && (isJust (library pkgDesc) && (notNull . exposedModules . fromJust . library) pkgDesc || mkPackageName "haskell-gi" `elem` (buildDeps pkgdeps ++ setupDeps pkgdeps))
      baselibpkg = if binlib then ghc_name ++ "-%{name}" else "%{name}"

  when hasLibPkg $ do
    when (binlib && hasModules) $ do
      put $ "%package" +-+ subpkgParam Base
      putHdr "Summary" $ "Haskell" +-+ pkg_name +-+ "library"
      when common $
        putHdr "Requires" $ "%{name}-common" ++ versionRelease
      putNewline
      put $ "%description" +-+ subpkgParam Base
      put $ wrapGenDesc $ "This package provides the Haskell" +-+ pkg_name +-+ "shared library."
      sectionNewline
    let isa = "%{?_isa}"
    put $ "%package" +-+ subpkgParam Devel
    putHdr "Summary" $ "Haskell" +-+ pkg_name +-+ "library development files"
    when hasModules $ do
      putHdr "Provides" $ baselibpkg ++ "-static" ++ versionRelease
      putHdr "Provides" $ baselibpkg ++ "-static" ++ isa ++ versionRelease
    put "%if %{defined ghc_version}"
    putHdr "Requires" $ ghc_name ++ "-compiler = %{ghc_version}"
    put "%endif"
    when hasModules $
      putHdr "Requires" $ baselibpkg ++ isa ++ versionRelease
    unless (null $ clibDeps pkgdeps ++ pkgcfgDeps pkgdeps) $ do
      put "# Begin cabal-rpm deps:"
      mapM_ (putHdr "Requires") $ sort $ map (++ isa) (clibDeps pkgdeps) ++ pkgcfgDeps pkgdeps
      put "# End cabal-rpm deps"
    putNewline
    put $ "%description" +-+ subpkgParam Devel
    put $ wrapGenDesc $ "This package provides the Haskell" +-+ pkg_name +-+ "library development files."
    sectionNewline
    when hasModules $ do
      put "%if %{with haddock}"
      put $ "%package" +-+ subpkgParam Doc
      putHdr "Summary" $ "Haskell" +-+ pkg_name +-+ "library documentation"
      putHdr "BuildArch" "noarch"
      putHdr "Requires" $ ghc_name ++ "-filesystem"
      putNewline
      put $ "%description" +-+ subpkgParam Doc
      put $ wrapGenDesc $ "This package provides the Haskell" +-+ pkg_name +-+ "library documentation."
      put "%endif"
      {- HLINT ignore "Reduce duplication"-}
      sectionNewline
      put "%if %{with ghc_prof}"
      put $ "%package" +-+ subpkgParam Prof
      putHdr "Summary" $ "Haskell" +-+ pkg_name +-+ "profiling library"
      putHdr "Requires" $ baselibpkg ++ "-devel" ++ isa ++ versionRelease
      putHdr "Supplements" $ "(" ++ baselibpkg ++ "-devel and" +-+ ghc_name ++ "-prof)"
      putNewline
      put $ "%description" +-+ subpkgParam Prof
      put $ wrapGenDesc $ "This package provides the Haskell" +-+ pkg_name +-+ "profiling library."
      put "%endif"
      sectionNewline

  when hasSubpkgs $ do
    global "main_version" "%{version}"
    putNewline
    put "%if %{defined ghclibdir}"
    mapM_ (\p -> let mlic = lookup p missingLibsLicenses in put $ "%ghc_lib_subpackage" +-+ maybe "" ("-l" +-+) mlic +-+ subpkgMacro p) subpkgs
    put "%endif"
    putNewline
    global "version" "%{main_version}"
    sectionNewline

  put "%prep"
  put "# Begin cabal-rpm setup:"
  put $ "%setup -q" ++ (if pkgname /= name then " -n" +-+ pkgver else "") +-+
    (if hasSubpkgs then unwords (map (("-a" ++) . fst) $ number subpkgs) else  "")
  when revised $
    put $ (if revisedDOS
           then "dos2unix -k -n" else "cp -bp")
          +-+ "%{SOURCE" ++ show (1 + length subpkgs) ++ "}" +-+ pkg_name <.> "cabal"
  when autopatch $
    put "%autopatch -p1"
  put "# End cabal-rpm setup"
  sectionNewline

  let pkgType = if hasLibPkg then "lib" else "bin"

  put "%build"
  put "# Begin cabal-rpm build:"
  if standalone then do
    global "cabal_install" "%{_bindir}/cabal"
    put $ "%cabal_install update" +-+ (if isJust mwithghc then "-w ghc-%{ghc_major}" else "")
    put "%if %{defined rhel} && 0%{?rhel} < 9"
    put "%cabal_install sandbox init"
    -- FIXME support mwithghc?
    put "%cabal_install install"
    put "%endif"
  else do
    when hasSubpkgs $
      put "%ghc_libs_build %{subpkgs}"
    put $ "%ghc_" ++ pkgType ++ "_build" ++
          if hasLibPkg && not hasModules then "_without_haddock" else []
  put "# End cabal-rpm build"
  sectionNewline

  put "%install"
  put "# Begin cabal-rpm install"
  if standalone then do
    put "mkdir -p %{buildroot}%{_bindir}"
    put "%if %{defined fedora} || 0%{?rhel} >= 9"
    put "%ghc_set_gcc_flags"
    put $ "%cabal_install install" +-+ (if isJust mwithghc then "-w ghc-%{ghc_major}" else "") +-+ "--install-method=copy --enable-executable-stripping --installdir=%{buildroot}%{_bindir}"
    put "%else"
    put "for i in .cabal-sandbox/bin/*; do"
    put "strip -s -o %{buildroot}%{_bindir}/$(basename $i) $i"
    put "done"
    put "%endif"
  else do
    when hasSubpkgs $
      put "%ghc_libs_install %{subpkgs}"
    put $ "%ghc_" ++ pkgType ++ "_install"

    unless (null dupdocs) $ do
      putNewline
      warn verbose $ "doc files found in datadir:" +-+ unwords dupdocs
      put $ "rm %{buildroot}%{_datadir}" </> pkgver </>
        case dupdocs of
           [hd] -> hd
           _ -> "{" ++ intercalate "," dupdocs ++ "}"

    when (hasLibPkg && not hasModules) $
      put "mv %{buildroot}%{_ghcdocdir}{,-devel}"

    when common $
      put "mv %{buildroot}%{_ghcdocdir}{,-common}"

  let execs = sort $ map exeName $ filter isBuildable $ executables pkgDesc
      execNaming p = let pn = unUnqualComponentName p in
                     if pn == name then "%{name}" else pn
      -- FIXME check for included file
      maybeGenerateBashCompletion exn =
        put $ "%{buildroot}%{_bindir}" </> exn +-+ "--bash-completion-script" +-+ exn +-+ "| sed s/filenames/default/ >" +-+ "%{buildroot}%{bash_completions_dir}" </> exn
      -- FIXME check for included file
      maybeGenerateManpage exn =
        put $ "help2man --no-info %{buildroot}%{_bindir}" </> exn +-+ ">" +-+ "%{buildroot}%{_mandir}/man1" </> exn <.> "1"

  when usesOptparse $ do
    putNewline
    put "set noclobber"
    put "mkdir -p %{buildroot}%{bash_completions_dir}"
    -- FIXME convert to for loop
    mapM_ (maybeGenerateBashCompletion . execNaming) execs
    putNewline
    put "mkdir -p %{buildroot}%{_mandir}/man1/"
    -- FIXME convert to for loop
    mapM_ (maybeGenerateManpage . execNaming) execs

  put "# End cabal-rpm install"
  sectionNewline

  when testable $ do
    put "%check"
    put "%if %{with tests}"
    when hasExec $
      put "PATH=%{buildroot}%{_bindir}:$PATH"
    put "%cabal_test"
    put "%endif"
    sectionNewline

  let license_macro = "%license"

  when hasExecPkg $ do
    put "%files"
    put "# Begin cabal-rpm files:"
    -- Add the license file to the main package only if it wouldn't
    -- otherwise be empty.
    unless common $ do
      mapM_ (\l -> put $ license_macro +-+ l) licensefiles
      unless (null docs) $
        put $ "%doc" +-+ unwords docs
    mapM_ (put . ("%{_bindir}" </>) . execNaming) execs
    unless (common || null datafiles) $
      put $ "%{_datadir}" </> pkgver
    when usesOptparse $ do
      mapM_ (put . ("%{bash_completions_dir}" </>) . execNaming) execs
      mapM_ (put . (\e -> "%{_mandir}/man1" </> e <.> "1*") . execNaming) execs
    put "# End cabal-rpm files"
    sectionNewline

  when common $ do
    put "%files common"
    put "# Begin cabal-rpm files:"
    mapM_ (\l -> put $ license_macro +-+ l) licensefiles
    unless (null docs) $
      put $ "%doc" +-+ unwords docs
    put $ "%{_datadir}" </> pkgver
    put "# End cabal-rpm files"
    sectionNewline

  when hasLibPkg $ do
    let filesParams lt = subpkgParam lt +-+ "-f" +-+ baselibpkg ++ pkgSuffix lt ++ ".files"
    when hasModules $ do
      put $ "%files" +-+ filesParams Base
      unless common $ do
        put "# Begin cabal-rpm files:"
        mapM_ (\l -> put $ license_macro +-+ l) licensefiles
        unless (null datafiles) $
          put $ "%{_datadir}" </> pkgver
        put "# End cabal-rpm files"
      sectionNewline

    put $ "%files" +-+ filesParams Devel
    -- put "# Begin cabal-rpm files:"
    unless hasModules $
      mapM_ (\l -> put $ license_macro +-+ l) licensefiles
    unless (common || null docs) $
      put $ "%doc" +-+ unwords docs
    unless binlib $
      mapM_ ((\p -> put $ "%{_bindir}" </> (if p == name then "%{pkg_name}" else p)) . unUnqualComponentName) execs
    -- put "# End cabal-rpm files"
    sectionNewline

    when hasModules $ do
      put "%if %{with haddock}"
      put $ "%files" +-+ filesParams Doc
      mapM_ (\l -> put $ license_macro +-+ l) licensefiles
      put "%endif"
      sectionNewline
      put "%if %{with ghc_prof}"
      put $ "%files" +-+ filesParams Prof
      put "%endif"
      sectionNewline

  put "%changelog"
  if autochangelog
  then put "%autochangelog"
  else do
    now <- getCurrentTime
    let date = formatTime defaultTimeLocale "%a %b %e %Y" now
    packager <- fromMaybe "Fedora Haskell SIG <haskell@lists.fedoraproject.org>" <$> cmdMaybe "rpmdev-packager" []
    put $ "*" +-+ date +-+ packager ++ " - " ++ version ++ "-" ++ release
    put $ "- spec file generated by cabal-rpm-" ++ V.showVersion Paths_cabal_rpm.version
  hClose h
  return outputFile

createSpecFile_ :: Bool -> Verbosity -> Flags -> Bool -> Bool -> PackageType
                -> Maybe (Maybe Stream) -> Maybe V.Version
                -> Maybe PackageVersionSpecifier -> IO ()
createSpecFile_ ignoreMissing verbose flags notestsuite force pkgtype subpkgStream mwithghc mpvs =
  void (createSpecFile ignoreMissing verbose flags False notestsuite force pkgtype subpkgStream mwithghc Nothing mpvs)

isBuildable :: Executable -> Bool
isBuildable exe = buildable $ buildInfo exe

sourceUrl :: String -> String
sourceUrl pv = "https://hackage.haskell.org/package" </> pv </> pv <.> ".tar.gz"

-- http://rosettacode.org/wiki/Word_wrap#Haskell
wordwrap :: Int -> String -> String
wordwrap maxlen = wrap_ 0 False . words
  where
    wrap_ _ _ [] = "\n"
    wrap_ pos eos (w:ws)
      -- at line start: put down the word no matter what
      | pos == 0 = w ++ wrap_ (pos + lw) endp ws
      | pos + lw + 1 > maxlen - 9 && eos = '\n':wrap_ 0 endp (w:ws)
      | pos + lw + 1 > maxlen = '\n':wrap_ 0 endp (w:ws)
      | otherwise = " " ++ w ++ wrap_ (pos + lw + 1) endp ws
      where
        lw = length w
        endp = last w == '.'

formatParagraphs :: String -> [String]
formatParagraphs = map (wordwrap 79) . paragraphs . lines
  where
    -- from http://stackoverflow.com/questions/930675/functional-paragraphs
    -- using split would be: map unlines . (Data.List.Split.splitWhen null)
    paragraphs :: [String] -> [String]
    paragraphs = map (unlines . filter notNull) . groupBy (const notNull)

number :: [a] -> [(String,a)]
number = zip (map show [(1::Int)..])

getPkgName :: Maybe FilePath -> String -> PackageDescription -> Bool
           -> IO (String, Bool)
getPkgName (Just spec) _ pkgDesc binary = do
  let name = display . pkgName $ package pkgDesc
      pkgname = takeBaseName spec
      hasLib = hasLibs pkgDesc
  return $ if name == pkgname || binary then (name, hasLib) else (pkgname, False)
getPkgName Nothing ghcname pkgDesc binary = do
  let name = display . pkgName $ package pkgDesc
      hasExec = hasExes pkgDesc
      hasLib = hasLibs pkgDesc
  return $ if binary || hasExec && not hasLib then (name, hasLib) else (ghcname ++ "-" ++ name, False)

quoteMacros :: String -> String
quoteMacros "" = ""
quoteMacros (c:cs) = (if c == '%' then "%%" else [c]) ++ quoteMacros cs

#if !MIN_VERSION_extra(1,6,8)
notNull :: [a] -> Bool
notNull = not . null
#endif

ifJust :: Maybe a -> (a -> IO Bool) -> IO Bool
ifJust Nothing _ = return False
ifJust (Just x) act = act x

htmlEntityDecode :: String -> String
htmlEntityDecode =
  TL.unpack . TLB.toLazyText . HD.htmlEncodedText . T.pack
