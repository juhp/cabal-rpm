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
                     subPackages, testsuiteDependencies')
import Header (headerOption, withSpecHead)
import PackageUtils (bringTarball, latestPackage, PackageData (..), prepare)
import SimpleCabal (buildable, mkPackageName, PackageDescription (..),
                    PackageIdentifier(..), PackageName, showVersion)
import SimpleCmd ((+-+),
#if MIN_VERSION_simple_cmd(0,2,2)
                  grep,
#endif
                  cmdMaybe,
                  grep_, removePrefix)
import Stackage (defaultLTS)
import Types

import Control.Monad.Extra

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif

import Data.Char        (toUpper)
import Data.List.Extra
import Data.Maybe       (isJust, fromMaybe, fromJust)
import Data.Time.Clock  (getCurrentTime)
import Data.Time.Format (formatTime)
import qualified Data.Version as V

import Distribution.Text (display)
import Distribution.License  (License (..)
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,2,0)
                             , licenseFromSPDX
#endif
                             )

import Distribution.PackageDescription (
                                        Executable (buildInfo),
                                        Library (exposedModules), exeName,
                                        hasExes, hasLibs,
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,2,0)
                                        license,
#endif
                                       )
import Distribution.Simple.Utils (warn)

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
#endif
import Distribution.Verbosity (Verbosity)

import System.Directory (doesFileExist)
import System.IO     (IOMode (..), hClose, hPutStrLn, openFile)
#if defined(MIN_VERSION_time) && MIN_VERSION_time(1,5,0)
import Data.Time.Format (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif
import System.FilePath (takeBaseName, (</>), (<.>))

import qualified Paths_cabal_rpm (version)

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(3,2,0)
import qualified Distribution.Utils.ShortText as ST (fromShortText)
#endif


#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
#else
unUnqualComponentName :: String -> String
unUnqualComponentName = id
#endif

-- FIXME use datatype for options
createSpecFile :: Bool -> Bool -> Bool -> Verbosity -> Flags -> Bool -> Bool
               -> PackageType -> Maybe (Maybe Stream) -> Maybe FilePath
               -> Maybe PackageVersionSpecifier
               -> IO FilePath
createSpecFile keep revise ignoreMissing verbose flags testsuite force pkgtype subpkgStream mdest mpvs = do
  pkgdata <- prepare flags mpvs revise keep
  let mspec = case pkgtype of
                SpecFile f -> Just f
                _ -> specFilename pkgdata
      docs = docFilenames pkgdata
      licensefiles = licenseFilenames pkgdata
      pkgDesc = packageDesc pkgdata
      pkgid = package pkgDesc
      name = display $ pkgName pkgid

  standalone <-
    if pkgtype == StandalonePkg then return True
    else
      case mspec of
        Nothing -> return False
        Just spec -> do
          specExists <- doesFileExist spec
          if specExists then
            withSpecHead spec $ return . ("--standalone" `elem`)
            else return False

  subpackage <-
    if isJust subpkgStream
    then
      if pkgtype == StandalonePkg
      then do
        warn verbose "ignoring --subpackage for --standalone"
        return False
      else return True
    else
      case mspec of
        Nothing -> return False
        Just spec -> do
          specExists <- doesFileExist spec
          if specExists then
            withSpecHead spec $ return . ("--subpackage" `elem`)
            else return False

  let hasExec = hasExes pkgDesc
      hasLib = hasLibs pkgDesc
      hasLibPkg = hasLib && not standalone
      dupdocs = docs `intersect` dataFiles pkgDesc
      datafiles = dataFiles pkgDesc \\ dupdocs
  (pkgname, binlib) <- getPkgName mspec pkgDesc (pkgtype == BinaryPkg || standalone)
  let pkg_name = if pkgname == name then "%{name}" else "%{pkg_name}"
      basename | binlib = "%{pkg_name}"
               | hasExecPkg = name
               | otherwise = "ghc-%{pkg_name}"
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

  mstream <- case pvsStream =<< mpvs of
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
      subpkgParam lt = if binlib then "-n ghc-%{name}" ++ pkgSuffix lt else show lt

  put $ "# generated by cabal-rpm-" ++ V.showVersion Paths_cabal_rpm.version
    +-+ unwords (["--standalone" | standalone] ++ ["--stream " ++ showStream (fromJust mstream) | isJust mstream, mstream /= Just defaultLTS] ++ ["--subpackage" | subpackage])
  put "# https://docs.fedoraproject.org/en-US/packaging-guidelines/Haskell/"
  putNewline

  -- Some packages conflate the synopsis and description fields.  Ugh.
  let syn =
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(3,2,0)
        ST.fromShortText $
#endif
        synopsis pkgDesc
  when (null syn) $
    warn verbose "this package has no synopsis."
  let initialCapital (c:cs) = toUpper c:cs
      initialCapital [] = []
      syn' = if null syn
             then "Haskell" +-+ name +-+ "package"
             else (unwords . lines . initialCapital) syn
      summary = dropWhileEnd (== '.') syn'
  when (length ("Summary     : " ++ syn') > 79) $
    warn verbose "this package has a long synopsis."

  let descr =
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(3,2,0)
        ST.fromShortText $
#endif
        description pkgDesc
  when (null descr) $
    warn verbose "this package has no description."
  let descLines = (formatParagraphs . initialCapital . filterSymbols . finalPeriod) $ if null descr then syn' else descr
      finalPeriod cs = case last cs of
                         '.' -> cs
                         '\n' -> finalPeriod $ init cs
                         _ -> cs ++ "."
      filterSymbols (c:cs) =
        if c `notElem` "@\\" then c: filterSymbols cs
        else case c of
          '@' -> '\'': filterSymbols cs
          '\\' -> head cs: filterSymbols (tail cs)
          _ -> c: filterSymbols cs
      filterSymbols [] = []

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
    putNewline

  let pkgver = if hasLib then "%{pkgver}" else pkg_name ++ "-%{version}"

  -- FIXME sort by build order
  -- FIXME recursive missingdeps
  missingLibs <- do
    subs <- if subpackage then subPackages mspec pkgDesc else return []
    miss <- if subpackage || standalone then missingLibraries pkgDesc else return []
    return $ nub (subs ++ miss) \\ droppedDeps
  subpkgs <- if subpackage then
    mapM (getsubpkgMacro (streamSubpackage mstream subpkgStream) mspec >=>
          \(m,pv) -> global m (display pv) >> return ("%{" ++ m ++ "}")) missingLibs
    else return []
  let hasSubpkgs = subpkgs /= []
  when hasSubpkgs $ do
    global "subpkgs" $ unwords subpkgs
    putNewline

  let (testsuiteDeps,testsuiteTools) = testsuiteDependencies' pkgDesc
  missTestDeps <- filterM (notSrcOrInst . RpmHsLib Devel) testsuiteDeps
  let testable = notNull testsuiteDeps && not standalone && (null missTestDeps || testsuite)
  if testable then do
    put "%bcond_without tests"
    putNewline
    else unless (null testsuiteDeps || standalone) $ do
         put $ "# testsuite missing deps: " ++ unwords (map display missTestDeps)
         putNewline
  let version = display $ pkgVersion pkgid
      release = "1"
  revised <-
    if isJust $ lookup "x-revision" (customFieldsPD pkgDesc)
    then return True
    else do
      if revise then do
        let local = display pkgid <.> "cabal"
        have <- doesFileExist local
        if have then grep_ "x-revision" local else return False
      else return False
  putHdr "Name" (if binlib then "%{pkg_name}" else basename)
  putHdr "Version" version
  when hasSubpkgs $
    put "# can only be reset when all subpkgs bumped"
  putHdr "Release" $ release ++ "%{?dist}"
  putHdr "Summary" summary
  putNewline
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,2,0)
#else
  let licenseFromSPDX = id
#endif
  putHdr "License" $ (showLicense . licenseFromSPDX . license) pkgDesc
  putHdr "Url" $ "https://hackage.haskell.org/package" </> pkg_name
  put "# Begin cabal-rpm sources:"
  putHdr "Source0" $ sourceUrl pkgver
  mapM_ (\ (n,p) -> putHdr ("Source" ++ n) (sourceUrl p)) $ number subpkgs
  when revised $
    putHdr ("Source" ++ show (1 + length subpkgs)) $ "https://hackage.haskell.org/package" </> pkgver </> pkg_name <.> "cabal" ++ "#" </> pkgver <.> "cabal"
  put "# End cabal-rpm sources"
  putNewline
  put "# Begin cabal-rpm deps:"
  when revised $ putHdr "BuildRequires" "dos2unix"
  when (mkPackageName "Cabal" `notElem` buildDeps pkgdeps || not hasLib || notNull (setupDeps pkgdeps)) $ do
--    put "# Setup"
    when (mkPackageName "Cabal" `notElem` buildDeps pkgdeps) $
      putHdr "BuildRequires" "ghc-Cabal-devel"
    mapM_ (\ d -> (if d `elem` missingLibs then putHdrComment else putHdr) "BuildRequires" (showRpm (RpmHsLib Devel d))) $ setupDeps pkgdeps
  putHdr "BuildRequires" $ "ghc-rpm-macros" ++ (if hasSubpkgs then "-extra" else "")

  unless (null (buildDeps pkgdeps)) $ do
--    put "# Build"
    let metaPackages = map mkPackageName ["haskell-gi-overloading"]
        ghcLibDep d | d `elem` metaPackages = RpmHsLib Devel d
                    | otherwise = (RpmHsLib $ if standalone then Devel else if hasLibPkg then Prof else Static) d
    mapM_ (\ d -> (if d `elem` missingLibs then putHdrComment else putHdr) "BuildRequires" (showRpm (ghcLibDep d))) $ sort $ buildDeps pkgdeps
  let otherdeps = sort $ toolDeps pkgdeps ++ clibDeps pkgdeps ++ pkgcfgDeps pkgdeps
  unless (null otherdeps) $ do
--    put "# Other"
    missingOthers <- missingOtherPkgs pkgDesc
    mapM_ (\ d -> (if d `elem` missingOthers then putHdrComment else putHdr) "BuildRequires" d) otherdeps
    -- -- for ghc < 7.8:
    -- when (epel7 &&
    --       any (\ d -> d `elem` map showDep ["template-haskell", "hamlet"]) deps) $
    --   putHdr "ExclusiveArch" "%{ghc_arches_with_ghci}"
  let testDeps = testsuiteDeps \\ (mkPackageName "Cabal" : (buildDeps pkgdeps ++ setupDeps pkgdeps))
  when (testable && notNull testDeps) $ do
    put "%if %{with tests}"
    mapM_ (putHdr "BuildRequires") $ (sort . map showRpm) (map (RpmHsLib Devel) testDeps ++ map RpmOther testsuiteTools)
    put "%endif"

  let common = binlib && datafiles /= [] && not standalone
      versionRelease = " = %{version}-%{release}"
  when common $
    putHdr "Requires" $ "%{name}-common" ++ versionRelease
  when (standalone || subpackage) $ do
    when standalone $
      putHdr "BuildRequires" "cabal-install > 1.18"
    unless (null missingLibs || ignoreMissing) $ do
      putStrLn "checking for deps of missing dependencies:"
      let deptype = if standalone then Devel else Prof
      forM_ missingLibs $ \ pkg -> do
        more <- packageDeps flags (streamSubpackage mstream subpkgStream) pkg
        let moredeps = sort $ more \\ (buildDeps pkgdeps ++ missingLibs)
        unless (null moredeps) $ do
          put $ "# for missing dep '" ++ display pkg ++ "':"
          mapM_ (\ d -> (if d `elem` missingLibs then putHdrComment else putHdr) "BuildRequires" (showRpm (RpmHsLib deptype d))) moredeps
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
      baselibpkg = if binlib then "ghc-%{name}" else "%{name}"

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
    putHdr "Requires" "ghc-compiler = %{ghc_version}"
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
      putHdr "Supplements" $ "(" ++ baselibpkg ++ "-devel and ghc-prof)"
      putNewline
      put $ "%description" +-+ subpkgParam Prof
      put $ wrapGenDesc $ "This package provides the Haskell" +-+ pkg_name +-+ "profiling library."
      put "%endif"
      sectionNewline

  when hasSubpkgs $ do
    global "main_version" "%{version}"
    putNewline
    put "%if %{defined ghclibdir}"
    mapM_ (\p -> put $ "%ghc_lib_subpackage" +-+ p) subpkgs
    put "%endif"
    putNewline
    global "version" "%{main_version}"
    sectionNewline

  put "%prep"
  put "# Begin cabal-rpm setup:"
  put $ "%setup -q" ++ (if pkgname /= name then " -n" +-+ pkgver else "") +-+
    (if hasSubpkgs then unwords (map (("-a" ++) . fst) $ number subpkgs) else  "")
  when revised $
    put $ "dos2unix -k -n %{SOURCE" ++ show (1 + length subpkgs) ++ "}" +-+ pkg_name <.> "cabal"
  put "# End cabal-rpm setup"
  sectionNewline

  let pkgType = if hasLibPkg then "lib" else "bin"

  put "%build"
  put "# Begin cabal-rpm build:"
  if standalone then do
    put "cabal update"
    put "%if 0%{fedora} < 33"
    put "cabal sandbox init"
    put "cabal install"
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
    put "%if 0%{fedora} >= 33"
    put "cabal install --install-method=copy --installdir=%{buildroot}%{_bindir}"
    put "%else"
    put "for i in .cabal-sandbox/bin/*; do"
    put "strip -s -o %{buildroot}%{_bindir}/$(basename $i) $i"
    put "done"
    put "%endif"
  else do
    when hasSubpkgs $
      put "%ghc_libs_install %{subpkgs}"
    put $ "%ghc_" ++ pkgType ++ "_install"

    when hasSubpkgs $
      put $ "%ghc_fix_rpath" +-+ pkgver

    unless (null dupdocs) $ do
      putNewline
      warn verbose $ "doc files found in datadir:" +-+ unwords dupdocs
      put $ "rm %{buildroot}%{_datadir}" </> pkgver </>
        case length dupdocs of
           1 -> head dupdocs
           _ -> "{" ++ intercalate "," dupdocs ++ "}"

    when (hasLibPkg && not hasModules) $
      put "mv %{buildroot}%{_ghcdocdir}{,-devel}"

    when common $
      put "mv %{buildroot}%{_ghcdocdir}{,-common}"

  let execs = sort $ map exeName $ filter isBuildable $ executables pkgDesc
      execNaming p = let pn = unUnqualComponentName p in
                     if pn == name then "%{name}" else pn
      bashCompletions = hasExecPkg && any ((`elem` buildDeps pkgdeps) . mkPackageName) ["optparse-applicative","simple-cmd-args"]

  when bashCompletions $ do
    put "mkdir -p %{buildroot}%{_datadir}/bash-completion/completions/"
    mapM_ (\ ex -> let exn = execNaming ex in put ("%{buildroot}%{_bindir}" </> exn ++ " --bash-completion-script " ++ exn ++ " > %{buildroot}%{_datadir}/bash-completion/completions" </> exn)) execs

  put "# End cabal-rpm install"
  sectionNewline

  when testable $ do
    put "%check"
    put "%cabal_test"
    sectionNewline

  let license_macro = "%license"

  when hasExecPkg $ do
    put "%files"
    put "# Begin cabal-rpm files:"
    -- Add the license file to the main package only if it wouldn't
    -- otherwise be empty.
    unless common $ do
      mapM_ (\ l -> put $ license_macro +-+ l) licensefiles
      unless (null docs) $
        put $ "%doc" +-+ unwords docs
    mapM_ (put . ("%{_bindir}" </>) . execNaming) execs
    unless (common || null datafiles) $
      put $ "%{_datadir}" </> pkgver
    when bashCompletions $
      mapM_ (put . ("%{_datadir}/bash-completion/completions" </>) . execNaming) execs
    put "# End cabal-rpm files"
    sectionNewline

  when common $ do
    put "%files common"
    put "# Begin cabal-rpm files:"
    mapM_ (\ l -> put $ license_macro +-+ l) licensefiles
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
        mapM_ (\ l -> put $ license_macro +-+ l) licensefiles
        unless (null datafiles) $
          put $ "%{_datadir}" </> pkgver
        put "# End cabal-rpm files"
      sectionNewline

    put $ "%files" +-+ filesParams Devel
    -- put "# Begin cabal-rpm files:"
    unless hasModules $
      mapM_ (\ l -> put $ license_macro +-+ l) licensefiles
    unless (common || null docs) $
      put $ "%doc" +-+ unwords docs
    unless binlib $
      mapM_ ((\ p -> put $ "%{_bindir}" </> (if p == name then "%{pkg_name}" else p)) . unUnqualComponentName) execs
    -- put "# End cabal-rpm files"
    sectionNewline

    when hasModules $ do
      put "%if %{with haddock}"
      put $ "%files" +-+ filesParams Doc
      mapM_ (\ l -> put $ license_macro +-+ l) licensefiles
      put "%endif"
      sectionNewline
      put "%if %{with ghc_prof}"
      put $ "%files" +-+ filesParams Prof
      put "%endif"
      sectionNewline

  put "%changelog"
  now <- getCurrentTime
  let date = formatTime defaultTimeLocale "%a %b %e %Y" now
  packager <- fromMaybe "Fedora Haskell SIG <haskell@lists.fedoraproject.org>" <$> cmdMaybe "rpmdev-packager" []
  put $ "*" +-+ date +-+ packager ++ " - " ++ version ++ "-" ++ release
  put $ "- spec file generated by cabal-rpm-" ++ V.showVersion Paths_cabal_rpm.version
  hClose h
  return outputFile

createSpecFile_ :: Bool -> Verbosity -> Flags -> Bool -> Bool -> PackageType
                -> Maybe (Maybe Stream) -> Maybe PackageVersionSpecifier -> IO ()
createSpecFile_ ignoreMissing verbose flags testsuite force pkgtype subpkgStream mpvs =
  void (createSpecFile True True ignoreMissing verbose flags testsuite force pkgtype subpkgStream Nothing mpvs)

isBuildable :: Executable -> Bool
isBuildable exe = buildable $ buildInfo exe

showLicense :: License -> String
showLicense (GPL Nothing) = "GPL+"
showLicense (GPL (Just ver)) = "GPLv" ++ showVersion ver ++ "+"
showLicense (LGPL Nothing) = "LGPLv2+"
showLicense (LGPL (Just ver)) = "LGPLv" ++ [head $ showVersion ver] ++ "+"
showLicense BSD3 = "BSD"
showLicense BSD4 = "BSD"
showLicense MIT = "MIT"
showLicense PublicDomain = "Public Domain"
showLicense AllRightsReserved = "Proprietary"
showLicense OtherLicense = "Unknown"
-- FIXME
showLicense (UnknownLicense l) = removePrefix "LicenseRef" l
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,16,0)
showLicense (Apache Nothing) = "ASL ?"
showLicense (Apache (Just ver)) = "ASL" +-+ showVersion ver
#endif
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,18,0)
showLicense (AGPL Nothing) = "AGPLv?"
showLicense (AGPL (Just ver)) = "AGPLv" ++ showVersion ver
#endif
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,20,0)
showLicense BSD2 = "BSD"
showLicense (MPL ver) = "MPLv" ++ showVersion ver
#endif
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,22,0)
showLicense ISC = "ISC"
showLicense UnspecifiedLicense = "Unspecified license!"
#endif

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

getsubpkgMacro :: Maybe Stream -> Maybe FilePath -> PackageName
               -> IO (String, PackageIdentifier)
getsubpkgMacro mstream mspec pkg = do
  let macro = filter (/= '-') $ display pkg
  pkgid <- latestPackage mstream pkg
  bringTarball pkgid False mspec
  return (macro, pkgid)

number :: [a] -> [(String,a)]
number = zip (map show [(1::Int)..])

getPkgName :: Maybe FilePath -> PackageDescription -> Bool -> IO (String, Bool)
getPkgName (Just spec) pkgDesc binary = do
  let name = display . pkgName $ package pkgDesc
      pkgname = takeBaseName spec
      hasLib = hasLibs pkgDesc
  return $ if name == pkgname || binary then (name, hasLib) else (pkgname, False)
getPkgName Nothing pkgDesc binary = do
  let name = display . pkgName $ package pkgDesc
      hasExec = hasExes pkgDesc
      hasLib = hasLibs pkgDesc
  return $ if binary || hasExec && not hasLib then (name, hasLib) else ("ghc-" ++ name, False)

quoteMacros :: String -> String
quoteMacros "" = ""
quoteMacros (c:cs) = (if c == '%' then "%%" else [c]) ++ quoteMacros cs

#if !MIN_VERSION_simple_cmd(0,2,2)
grep :: String -> FilePath -> IO [String]
grep pat file = do
  mres <- cmdMaybe "grep" [pat, file]
  return $ maybe [] lines mres
#endif

streamSubpackage :: Maybe Stream -> Maybe (Maybe Stream) -> Maybe Stream
streamSubpackage _ (Just (Just stream)) = Just stream
streamSubpackage mstream _ = mstream

#if !MIN_VERSION_extra(1,6,8)
notNull :: [a] -> Bool
notNull = not . null
#endif
