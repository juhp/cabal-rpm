{-# LANGUAGE CPP #-}

-- |
-- Module      :  Commands.Spec
-- Copyright   :  (C) 2007-2008  Bryan O'Sullivan
--                (C) 2012-2019  Jens Petersen
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

import Dependencies (notInstalled, missingPackages, packageDependencies,
                     subPackages, testsuiteDependencies)
import Types
import PackageUtils (bringTarball, getPkgName, latestPackage,
                     PackageData (..), packageName, packageVersion,
                     prepare, prettyShow, stripPkgDevel)
import SimpleCmd ((+-+))

import Control.Monad    (filterM, unless, void, when, (>=>))

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
--import Control.Applicative ((<$>))
#endif

import Data.Char        (toUpper)
import Data.List        (groupBy, intercalate, intersect, isPrefixOf,
                         nub, sort, (\\))
import Data.Maybe       (isJust, fromMaybe, fromJust)
import Data.Time.Clock  (getCurrentTime)
import Data.Time.Format (formatTime)
import Data.Version (showVersion)

import Distribution.License  (License (..)
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,2,0)
                             , licenseFromSPDX
#endif
                             )

import Distribution.PackageDescription (BuildInfo (..), PackageDescription (..),
                                        Executable (..),
                                        Library (..), exeName, hasExes, hasLibs,
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


rstrip :: (Char -> Bool) -> String -> String
rstrip p = reverse . dropWhile p . reverse

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
#else
unUnqualComponentName :: String -> String
unUnqualComponentName = id
#endif

createSpecFile :: Verbosity -> Flags -> Bool -> PackageType -> Bool -> Stream -> Maybe FilePath -> Maybe Package -> IO FilePath
createSpecFile verbose flags force pkgtype subpackage stream mdest mpkg = do
  pkgdata <- prepare flags stream mpkg True
  let mspec = case pkgtype of
                SpecFile f -> Just f
                _ -> specFilename pkgdata
      docs = docFilenames pkgdata
      licensefiles = licenseFilenames pkgdata
      pkgDesc = packageDesc pkgdata
      pkg = package pkgDesc
      name = packageName pkg
      standalone = pkgtype == StandalonePkg
      hasExec = hasExes pkgDesc
      hasLib = hasLibs pkgDesc
      hasLibPkg = hasLib && not standalone
      dupdocs = docs `intersect` dataFiles pkgDesc
      datafiles = dataFiles pkgDesc \\ dupdocs
  (pkgname, binlib) <- getPkgName mspec pkgDesc (pkgtype == BinaryPkg || standalone)
  let pkg_name = if pkgname == name then "%{name}" else "%{pkg_name}"
      basename | binlib = "%{pkg_name}"
               | hasExecPkg = name
               | otherwise = "ghc-%{pkg_name}"
      specFile = fromMaybe "" mdest </> pkgname <.> "spec"
      hasExecPkg = binlib || (hasExec && not hasLibPkg)
  -- run commands before opening file to prevent empty file on error
  -- maybe shell commands should be in a monad or something
  (deps, tools, clibs, pkgcfgs) <- packageDependencies pkgDesc
  let testsuiteDeps = testsuiteDependencies pkgDesc name

  specAlreadyExists <- doesFileExist specFile
  let specFile' = specFile ++ if not force && specAlreadyExists then ".cblrpm" else ""
  if specAlreadyExists
    then warn verbose $ specFile +-+ "exists:" +-+ (if force then "forcing overwrite" else "creating") +-+ specFile'
    else do
    let realdir dir = "cblrpm." `isPrefixOf` takeBaseName dir
    when (maybe True realdir mdest) $
      putStrLn pkgname

  h <- openFile specFile' WriteMode
  let putHdr hdr val = hPutStrLn h (hdr ++ ":" ++ padding hdr ++ val)
      padding hdr = replicate (14 - length hdr) ' ' ++ " "
      putHdrComment hdr val = putHdr ('#':hdr) (' ':val)
      putNewline = hPutStrLn h ""
      sectionNewline = putNewline >> putNewline
      put = hPutStrLn h
      global m v = put $ "%global" +-+ m +-+ v
      ghcPkg = if binlib then "-n ghc-%{name}" else ""
      ghcPkgDevel = if binlib then "-n ghc-%{name}-devel" else "devel"

  put $ "# generated by cabal-rpm-" ++ showVersion Paths_cabal_rpm.version
    +-+ unwords (["--standalone" | standalone] ++ ["--subpackage" | subpackage])
  put "# https://fedoraproject.org/wiki/Packaging:Haskell"
  putNewline

  -- Some packages conflate the synopsis and description fields.  Ugh.
  let syn = synopsis pkgDesc
  when (null syn) $
    warn verbose "this package has no synopsis."
  let initialCapital (c:cs) = toUpper c:cs
      initialCapital [] = []
  let syn' = if null syn
             then "Haskell" +-+ name +-+ "package"
             else (unwords . lines . initialCapital) syn
  let summary = rstrip (== '.') syn'
  when (length ("Summary     : " ++ syn') > 79) $
    warn verbose "this package has a long synopsis."

  let descr = description pkgDesc
  when (null descr) $
    warn verbose "this package has no description."
  let descLines = (formatParagraphs . initialCapital . filterSymbols . finalPeriod) $ if null descr then syn' else descr
      finalPeriod cs = if last cs == '.' then cs else cs ++ "."
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
  missing <- do
    subs <- if subpackage then subPackages mspec pkgDesc else return []
    miss <- if subpackage || standalone then missingPackages pkgDesc else return []
    return $ nub (subs ++ miss)
  subpkgs <- if subpackage then
    mapM ((getsubpkgMacro stream specFile >=>
           \(m,pv) -> global m pv >> return ("%{" ++ m ++ "}"))
           . stripPkgDevel) missing
    else return []
  let hasSubpkgs = subpkgs /= []
  when hasSubpkgs $ do
    global "subpkgs" $ unwords subpkgs
    putNewline

  unless (null testsuiteDeps) $ do
    missTestDeps <- filterM notInstalled testsuiteDeps
    put $ "%bcond_" ++ (if null missTestDeps then "without" else "with") +-+ "tests"
    putNewline

  let version = packageVersion pkg
      release = "1"
      revised = isJust $ lookup "x-revision" (customFieldsPD pkgDesc)
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
  putHdr "BuildRequires" "ghc-Cabal-devel"
  putHdr "BuildRequires" $ "ghc-rpm-macros" ++ (if hasSubpkgs then "-extra" else "")

  let alldeps = sort $ deps ++ tools ++ clibs ++ pkgcfgs ++ ["chrpath" | hasSubpkgs]
  let testDeps = sort $ testsuiteDeps \\ deps
  unless (null $ alldeps ++ testDeps) $ do
    mapM_ (\ d -> (if d `elem` missing then putHdrComment else putHdr) "BuildRequires" d) alldeps
    -- -- for ghc < 7.8:
    -- when (epel7 &&
    --       any (\ d -> d `elem` map showDep ["template-haskell", "hamlet"]) deps) $
    --   putHdr "ExclusiveArch" "%{ghc_arches_with_ghci}"
    unless (null testDeps) $ do
      put "%if %{with tests}"
      mapM_ (putHdr "BuildRequires") testDeps
      put "%endif"

  let common = binlib && datafiles /= [] && not standalone
  when common $
    putHdr "Requires" "%{name}-common = %{version}-%{release}"
  put "# End cabal-rpm deps"
  when standalone $
    putHdr "BuildRequires" "cabal-install > 1.18"
  putNewline

  put "%description"
  mapM_ put descLines
  putNewline

  let wrapGenDesc = wordwrap (79 - max 0 (length pkgname - length pkg_name))

  when common $ do
    put "%package common"
    putHdr "Summary" $ pkg_name +-+ "common files"
    putHdr "BuildArch" "noarch"
    putNewline
    put "%description common"
    put $ wrapGenDesc $ "This package provides the" +-+ pkg_name +-+ "common data files."
    putNewline

  -- haskell-gi generates lib source files at configure time
  -- (stricly should also check for otherModules (autogenModules?)
  --  but libraries wihout exposedModules should be useless/redundant)
  let hasModules =
        hasLib && ((notNull . exposedModules . fromJust . library) pkgDesc || "ghc-haskell-gi-devel" `elem` deps)

  when hasLibPkg $ do
    when (binlib && hasModules) $ do
      put $ "%package" +-+ ghcPkg
      putHdr "Summary" $ "Haskell" +-+ pkg_name +-+ "library"
      when common $
        putHdr "Requires" "%{name}-common = %{version}-%{release}"
      putNewline
      put $ "%description" +-+ ghcPkg
      put $ wrapGenDesc $ "This package provides the Haskell" +-+ pkg_name +-+ "shared library."
      putNewline
    put $ "%package" +-+ ghcPkgDevel
    putHdr "Summary" $ "Haskell" +-+ pkg_name +-+ "library development files"
    putHdr "Provides" $ (if binlib then "ghc-%{name}" else "%{name}") ++ "-static = %{version}-%{release}"
    when hasModules $
      putHdr "Provides" $ (if binlib then "ghc-%{name}" else "%{name}") ++ "-doc" +-+ "= %{version}-%{release}"
    put "%if %{defined ghc_version}"
    putHdr "Requires" "ghc-compiler = %{ghc_version}"
    putHdr "Requires(post)" "ghc-compiler = %{ghc_version}"
    putHdr "Requires(postun)" "ghc-compiler = %{ghc_version}"
    put "%endif"
    let isa = "%{?_isa}"
    when hasModules $
      putHdr "Requires" $ (if binlib then "ghc-%{name}" else "%{name}") ++ isa +-+ "= %{version}-%{release}"
    unless (null $ clibs ++ pkgcfgs) $ do
      put "# Begin cabal-rpm deps:"
      mapM_ (putHdr "Requires") $ sort $ map (++ isa) clibs ++ pkgcfgs
      put "# End cabal-rpm deps"
    putNewline
    put $ "%description" +-+ ghcPkgDevel
    put $ wrapGenDesc $ "This package provides the Haskell" +-+ pkg_name +-+ "library development files."
    -- previous line ends in an extra newline
    putNewline

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
    put $ "cp -bp %{SOURCE" ++ show (1 + length subpkgs) ++ "}" +-+ pkg_name <.> "cabal"
  put "# End cabal-rpm setup"
  sectionNewline

  put "%build"
  put "# Begin cabal-rpm build:"
  when hasSubpkgs $
    put "%ghc_libs_build %{subpkgs}"
  when standalone $ do
    global "cabal" "cabal"
    put "%cabal update"
    put "%cabal sandbox init"
    put "%cabal install --only-dependencies"
  let pkgType = if hasLibPkg then "lib" else "bin"
  put $ "%ghc_" ++ pkgType ++ "_build" ++
        if hasLibPkg && not hasModules then "_build_without_haddock" else []
  put "# End cabal-rpm build"
  sectionNewline

  put "%install"
  put "# Begin cabal-rpm install"
  when hasSubpkgs $
    put "%ghc_libs_install %{subpkgs}"
  put $ "%ghc_" ++ pkgType ++ "_install"

  when hasSubpkgs $
    put $ "%ghc_fix_rpath" +-+ pkgver

  unless (null dupdocs) $ do
    putNewline
    putStrLn $ "Warning: doc files found in datadir:" +-+ unwords dupdocs
    put $ "rm %{buildroot}%{_datadir}" </> pkgver </>
      case length dupdocs of
         1 -> head dupdocs
         _ -> "{" ++ intercalate "," dupdocs ++ "}"

  when (hasLibPkg && not hasModules) $
    put "mv %{buildroot}%{_ghcdocdir}{,-devel}"

  when common $
    put "mv %{buildroot}%{_ghcdocdir}{,-common}"

  put "# End cabal-rpm install"

  when (standalone && hasLib) $ do
    -- can be dropped with ghc-rpm-macros-1.9.8
    put "find %{buildroot}%{_libdir} -name 'libHS%{pkgver}-*.so' -delete"
    put "rm -r %{buildroot}%{ghclibdir}"

  sectionNewline

  unless (null testsuiteDeps) $ do
    put "%check"
    put "%cabal_test"
    sectionNewline

  when hasLibPkg $ do
    put "%if 0%{?fedora} > 30"
    put "%else"
    put $ "%post" +-+ ghcPkgDevel
    put "%ghc_pkg_recache"
    sectionNewline

    put $ "%postun" +-+ ghcPkgDevel
    put "%ghc_pkg_recache"
    put "%endif"
    sectionNewline

  let license_macro = "%license"
  let execs = sort $ map exeName $ filter isBuildable $ executables pkgDesc

  when hasExecPkg $ do
    put "%files"
    put "# Begin cabal-rpm files:"
    -- Add the license file to the main package only if it wouldn't
    -- otherwise be empty.
    unless common $ do
      mapM_ (\ l -> put $ license_macro +-+ l) licensefiles
      unless (null docs) $
        put $ "%doc" +-+ unwords docs
    mapM_ ((\ p -> put $ "%{_bindir}" </> (if p == name then "%{name}" else p)) . unUnqualComponentName) execs
    unless (common || null datafiles) $
      put $ "%{_datadir}" </> pkgver
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
    let baseFiles = if binlib then "-f ghc-%{name}.files" else "-f %{name}.files"
        develFiles = if binlib then "-f ghc-%{name}-devel.files" else "-f %{name}-devel.files"
    when hasModules $ do
      put $ "%files" +-+ ghcPkg +-+ baseFiles
      put "# Begin cabal-rpm files:"
      unless common $
        mapM_ (\ l -> put $ license_macro +-+ l) licensefiles
      unless (common || null datafiles) $
        put $ "%{_datadir}" </> pkgver
      put "# End cabal-rpm files"
      sectionNewline

    put $ "%files" +-+ ghcPkgDevel +-+  develFiles
    -- put "# Begin cabal-rpm files:"
    unless hasModules $
      mapM_ (\ l -> put $ license_macro +-+ l) licensefiles
    unless (common || null docs) $
      put $ "%doc" +-+ unwords docs
    unless binlib $
      mapM_ ((\ p -> put $ "%{_bindir}" </> (if p == name then "%{pkg_name}" else p)) . unUnqualComponentName) execs
    -- put "# End cabal-rpm files"
    sectionNewline

  put "%changelog"
  now <- getCurrentTime
  let date = formatTime defaultTimeLocale "%a %b %e %Y" now
  put $ "*" +-+ date +-+ "Fedora Haskell SIG <haskell@lists.fedoraproject.org> - " ++ version ++ "-" ++ release
  put $ "- spec file generated by cabal-rpm-" ++ showVersion Paths_cabal_rpm.version
  hClose h
  return specFile'

createSpecFile_ :: Verbosity -> Maybe FilePath -> Flags -> Bool -> PackageType -> Bool -> Stream -> Maybe Package -> IO ()
createSpecFile_ verbose mdest flags force pkgtype subpackage stream mpkg =
  void (createSpecFile verbose flags force pkgtype subpackage stream mdest mpkg)

isBuildable :: Executable -> Bool
isBuildable exe = buildable $ buildInfo exe

showLicense :: License -> String
showLicense (GPL Nothing) = "GPL+"
showLicense (GPL (Just ver)) = "GPLv" ++ prettyShow ver ++ "+"
showLicense (LGPL Nothing) = "LGPLv2+"
showLicense (LGPL (Just ver)) = "LGPLv" ++ [head $ prettyShow ver] ++ "+"
showLicense BSD3 = "BSD"
showLicense BSD4 = "BSD"
showLicense MIT = "MIT"
showLicense PublicDomain = "Public Domain"
showLicense AllRightsReserved = "Proprietary"
showLicense OtherLicense = "Unknown"
showLicense (UnknownLicense l) = l
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,16,0)
showLicense (Apache Nothing) = "ASL ?"
showLicense (Apache (Just ver)) = "ASL" +-+ prettyShow ver
#endif
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,18,0)
showLicense (AGPL Nothing) = "AGPLv?"
showLicense (AGPL (Just ver)) = "AGPLv" ++ prettyShow ver
#endif
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,20,0)
showLicense BSD2 = "BSD"
showLicense (MPL ver) = "MPLv" ++ prettyShow ver
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

getsubpkgMacro :: Stream -> FilePath -> String -> IO (String, String)
getsubpkgMacro stream spec pkg = do
  let macro = filter (/= '-') pkg
  pkgver <- latestPackage stream pkg
  bringTarball pkgver False spec
  return (macro, pkgver)

number :: [a] -> [(String,a)]
number = zip (map show [(1::Int)..])

notNull :: [a] -> Bool
notNull = not . null
