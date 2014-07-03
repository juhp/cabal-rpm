{-# LANGUAGE CPP #-}

-- |
-- Module      :  Commands.Spec
-- Copyright   :  (C) 2007-2008  Bryan O'Sullivan
--                (C) 2012-2014  Jens Petersen
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
  createSpecFile
  ) where

import Dependencies (packageDependencies, showDep, testsuiteDependencies)
import PackageUtils (findPkgName, isScmDir, notInstalled, packageName,
                     packageVersion)
import Setup (RpmFlags (..))
import SysCmd ((+-+))

import Control.Monad    (filterM, when, unless)
import Data.Char        (toLower, toUpper)
import Data.List        (groupBy, isPrefixOf, isSuffixOf, sort, (\\))
import Data.Maybe       (fromMaybe, maybeToList)
import Data.Time.Clock  (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime)
import Data.Version     (showVersion)

import Distribution.License  (License (..))

import Distribution.Simple.Utils (notice, warn)

import Distribution.PackageDescription (PackageDescription (..), BuildInfo (..),
                                        Executable (..), Library (..),
                                        exeName, hasExes, hasLibs, withExe)

--import Distribution.Version (VersionRange, foldVersionRange')

import System.Directory (doesFileExist, getDirectoryContents)
import System.IO     (IOMode (..), hClose, hPutStrLn, openFile)
import System.Locale (defaultTimeLocale)
import System.FilePath (dropFileName, takeDirectory, (</>))

import qualified Paths_cabal_rpm (version)


defaultRelease :: FilePath -> UTCTime -> IO String
defaultRelease cabalPath now = do
  let pkgDir = takeDirectory cabalPath
  scmRepo <- isScmDir pkgDir
  return $ if scmRepo
           then formatTime defaultTimeLocale "0.%Y%m%d" now
           else "1"

rstrip :: (Char -> Bool) -> String -> String
rstrip p = reverse . dropWhile p . reverse

-- packageName :: PackageIdentifier -> String
-- packageName pkg = name
--   where PackageName name = pkgName pkg

-- packageVersion :: PackageIdentifier -> String
-- packageVersion pkg = (showVersion . pkgVersion) pkg

createSpecFile :: FilePath            -- ^pkg cabal file
               -> PackageDescription  -- ^pkg description
               -> RpmFlags            -- ^rpm flags
               -> Maybe FilePath      -- ^optional destdir
               -> IO ()
createSpecFile cabalPath pkgDesc flags mdest = do
  let pkg = package pkgDesc
      name = packageName pkg
      verbose = rpmVerbosity flags
      forceLib = hasLib && rpmLibrary flags
      hasExec = hasExes pkgDesc
      hasLib = hasLibs pkgDesc
  now <- getCurrentTime
  defRelease <- defaultRelease cabalPath now
  pkgname <- findPkgName pkgDesc flags
  let pkg_name = if pkgname == name then "%{name}" else "%{pkg_name}"
      basename | isBinLib = "%{pkg_name}"
               | hasExecPkg = name
               | otherwise = "ghc-%{pkg_name}"
      version = packageVersion pkg
      release = fromMaybe defRelease (rpmRelease flags)
      specFile = fromMaybe "" mdest </> pkgname ++ ".spec"
      isBinLib = not forceLib && hasLib && hasExec
      hasExecPkg = not forceLib && (isBinLib || hasExec && not hasLib)
  -- run commands before opening file to prevent empty file on error
  -- maybe shell commands should be in a monad or something
  (deps, tools, clibs, pkgcfgs, selfdep) <- packageDependencies pkgDesc name
  let testsuiteDeps = testsuiteDependencies pkgDesc name
  missTestDeps <- filterM notInstalled testsuiteDeps

  specAlreadyExists <- doesFileExist specFile
  let specFilename = specFile ++ if not (rpmForce flags) && specAlreadyExists then ".cblrpm" else ""
  when specAlreadyExists $
    notice verbose $ specFile +-+ "exists:" +-+ if rpmForce flags then "forcing overwrite" else "creating" +-+ specFilename
  h <- openFile specFilename WriteMode
  let putHdr hdr val = hPutStrLn h (hdr ++ ":" ++ padding hdr ++ val)
      padding hdr = replicate (14 - length hdr) ' ' ++ " "
      putNewline = hPutStrLn h ""
      put = hPutStrLn h
      putDef v s = put $ "%global" +-+ v +-+ s
      ghcPkg = if isBinLib then "-n ghc-%{name}" else ""
      ghcPkgDevel = if isBinLib then "-n ghc-%{name}-devel" else "devel"

  distro <- detectDistro
  let suse = distro == SUSE
  if not suse
    then put "# https://fedoraproject.org/wiki/Packaging:Haskell"
    else do
    let year = formatTime defaultTimeLocale "%Y" now
    put "#"
    put $ "# spec file for package " ++ pkgname
    put "#"
    put $ "# Copyright (c) " ++ year ++ " SUSE LINUX Products GmbH, Nuernberg, Germany."
    put "#"
    put "# All modifications and additions to the file contributed by third parties"
    put "# remain the property of their copyright owners, unless otherwise agreed"
    put "# upon. The license for this file, and modifications and additions to the"
    put "# file, is the same license as for the pristine package itself (unless the"
    put "# license for the pristine package is not an Open Source License, in which"
    put "# case the license is the MIT License). An \"Open Source License\" is a"
    put "# license that conforms to the Open Source Definition (Version 1.9)"
    put "# published by the Open Source Initiative."
    putNewline
    put "# Please submit bugfixes or comments via http://bugs.opensuse.org/"
    put "#"
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
  when hasLib $ do
    putDef "pkg_name" name
    putNewline

  unless (null testsuiteDeps) $ do
    put $ "%bcond_" ++ (if null missTestDeps then "without" else "with") +-+ "tests"
    putNewline

  let eCsources = concatMap (cSources . buildInfo) $ executables pkgDesc
  let lCsources = concatMap (cSources . libBuildInfo) $ maybeToList $ library pkgDesc
  when (null $ eCsources ++ lCsources) $ do
    put "# no useful debuginfo for Haskell packages without C sources"
    putDef "debug_package" "%{nil}"
    putNewline

  putHdr "Name" (if isBinLib then "%{pkg_name}" else basename)
  putHdr "Version" version
  putHdr "Release" $ release ++ (if suse then [] else "%{?dist}")
  putHdr "Summary" summary
  when suse $ putHdr "Group" " "
  putNewline
  putHdr "License" $ (showLicense distro . license) pkgDesc
  putHdr "Url" $ "http://hackage.haskell.org/package/" ++ pkg_name
  putHdr "Source0" $ "http://hackage.haskell.org/package/" ++ pkg_name ++ "-%{version}/" ++ pkg_name ++ "-%{version}.tar.gz"
  when suse $ putHdr "BuildRoot" "%{_tmppath}/%{name}-%{version}-build"
  putNewline
  putHdr "BuildRequires" "ghc-Cabal-devel"
  putHdr "BuildRequires" "ghc-rpm-macros"

  let isa = if suse then "" else "%{?_isa}"
  let alldeps = sort $ deps ++ tools ++ map (++ isa) clibs ++ pkgcfgs
  let extraTestDeps = sort $ testsuiteDeps \\ deps
  unless (null $ alldeps ++ extraTestDeps) $ do
    put "# Begin cabal-rpm deps:"
    mapM_ (putHdr "BuildRequires") alldeps
    when (any (\ d -> d `elem` map showDep ["template-haskell", "hamlet"]) deps) $
      putHdr "ExclusiveArch" "%{ghc_arches_with_ghci}"
    unless (null extraTestDeps) $ do
      put "%if %{with tests}"
      mapM_ (putHdr "BuildRequires") extraTestDeps
      put "%endif"
    put "# End cabal-rpm deps"

  putNewline

  put "%description"
  mapM_ put descLines
  putNewline

  let wrapGenDesc = wordwrap (79 - max 0 (length pkgname - length pkg_name))

  when hasLib $ do
    when isBinLib $ do
      put $ "%package" +-+ ghcPkg
      putHdr "Summary" $ "Haskell" +-+ pkg_name +-+ "library"
      when suse $ putHdr "Group" "System/Libraries"
      putNewline
      put $ "%description" +-+ ghcPkg
      put $ wrapGenDesc $ "This package provides the Haskell" +-+ pkg_name +-+ "shared library."
      putNewline
    put $ "%package" +-+ ghcPkgDevel
    putHdr "Summary" $ "Haskell" +-+ pkg_name +-+ "library development files"
    unless suse $
      putHdr "Provides" $ (if isBinLib then "ghc-%{name}" else "%{name}") ++ "-static = %{version}-%{release}"
    putHdr "Requires" "ghc-compiler = %{ghc_version}"
    putHdr "Requires(post)" "ghc-compiler = %{ghc_version}"
    putHdr "Requires(postun)" "ghc-compiler = %{ghc_version}"
    when suse $ putHdr "Group" "Development/Libraries/Other"
    putHdr "Requires" $ (if isBinLib then "ghc-%{name}" else "%{name}") ++ isa +-+ "= %{version}-%{release}"
    unless (null $ clibs ++ pkgcfgs) $ do
      put "# Begin cabal-rpm deps:"
      mapM_ (putHdr "Requires") $ sort $ map (++ isa) clibs ++ pkgcfgs
      put "# End cabal-rpm deps"
    putNewline
    put $ "%description" +-+ ghcPkgDevel
    put $ wrapGenDesc $ "This package provides the Haskell" +-+ pkg_name +-+ "library development files."
    putNewline

  put "%prep"
  put $ "%setup -q" ++ (if pkgname /= name then " -n %{pkg_name}-%{version}" else "")
  putNewline
  putNewline

  put "%build"
  let pkgType = if hasLib then "lib" else "bin"
  put $ "%ghc_" ++ pkgType ++ "_build"
  putNewline
  putNewline

  put "%install"
  put $ "%ghc_" ++ pkgType ++ "_install"
  when selfdep $ do
    putNewline
    put "%ghc_fix_dynamic_rpath %{name}"
  putNewline
  putNewline

  unless (null testsuiteDeps) $ do
    put "%check"
    put "%if %{with tests}"
    put "%cabal test"
    put "%endif"
    putNewline
    putNewline

  when hasLib $ do
    let putInstallScript = do
          put "%ghc_pkg_recache"
          putNewline
          putNewline
    put $ "%post" +-+ ghcPkgDevel
    putInstallScript
    put $ "%postun" +-+ ghcPkgDevel
    putInstallScript

  let licensefiles =
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,20,0)
        licenseFiles pkgDesc
#else
        [licenseFile pkgDesc]
#endif
  docs <- findDocs cabalPath licensefiles

  when hasExecPkg $ do
    put "%files"
    when suse $ put "%defattr(-,root,root,-)"
    -- Add the license file to the main package only if it wouldn't
    -- otherwise be empty.
    mapM_ (\ l -> put $ "%doc" +-+ l) licensefiles
    unless (null docs) $
      put $ "%doc" +-+ unwords docs

    withExe pkgDesc $ \exe ->
      let program = exeName exe in
      put $ "%{_bindir}/" ++ (if program == name then "%{name}" else program)
    unless (null (dataFiles pkgDesc)) $
      put "%{_datadir}/%{name}-%{version}"

    putNewline
    putNewline

  when hasLib $ do
    let baseFiles = if isBinLib then "-f ghc-%{name}.files" else "-f %{name}.files"
        develFiles = if isBinLib then "-f ghc-%{name}-devel.files" else "-f %{name}-devel.files"
    put $ "%files" +-+ ghcPkg +-+ baseFiles
    when suse $ put "%defattr(-,root,root,-)"
    mapM_ (\ l -> put $ "%doc" +-+ l) licensefiles
    -- be strict for now
--      unless (null (dataFiles pkgDesc) || isBinLib) $
--        put "%{_datadir}/%{pkg_name}-%{version}"
    putNewline
    putNewline
    put $ "%files" +-+ ghcPkgDevel +-+  develFiles
    when suse $ put "%defattr(-,root,root,-)"
    unless (null docs) $
      put $ "%doc" +-+ unwords docs
    when (not isBinLib && hasExec) $
      withExe pkgDesc $ \exe ->
      let program = exeName exe in
      put $ "%{_bindir}/" ++ (if program == name then "%{name}" else program)
    putNewline
    putNewline

  put "%changelog"
  unless suse $ do
    let date = formatTime defaultTimeLocale "%a %b %e %Y" now
    put $ "*" +-+ date +-+ "Fedora Haskell SIG <haskell@lists.fedoraproject.org> - " ++ version ++ "-" ++ release
    put $ "- spec file generated by cabal-rpm-" ++ showVersion Paths_cabal_rpm.version
  hClose h

findDocs :: FilePath -> [FilePath] -> IO [FilePath]
findDocs cabalPath licensefiles = do
  contents <- getDirectoryContents $ dropFileName cabalPath
  let docs = filter likely contents
  return $ if null licensefiles
           then docs
           else filter unlikely $ filter (`notElem` licensefiles) docs
  where names = ["author", "copying", "doc", "example", "licence", "license",
                 "readme", "todo"]
        likely name = let lowerName = map toLower name
                      in any (`isPrefixOf` lowerName) names
        unlikely name = not $ any (`isSuffixOf` name) ["~"]

showLicense :: Distro -> License -> String
showLicense Fedora (GPL Nothing) = "GPL+"
showLicense SUSE (GPL Nothing) = "GPL-1.0+"
showLicense Fedora (GPL (Just ver)) = "GPLv" ++ showVersion ver ++ "+"
showLicense SUSE (GPL (Just ver)) = "GPL-" ++ showVersion ver ++ "+"
showLicense Fedora (LGPL Nothing) = "LGPLv2+"
showLicense SUSE (LGPL Nothing) = "LGPL-2.0+"
showLicense Fedora (LGPL (Just ver)) = "LGPLv" ++ [head $ showVersion ver] ++ "+"
showLicense SUSE (LGPL (Just ver)) = "LGPL-" ++ [head $ showVersion ver] ++ "+"
showLicense Fedora BSD3 = "BSD"
showLicense SUSE BSD3 = "BSD-3-Clause"
showLicense Fedora BSD4 = "BSD"
showLicense SUSE BSD4 = "BSD-4-Clause"
showLicense _ MIT = "MIT"
showLicense Fedora PublicDomain = "Public Domain"
showLicense SUSE PublicDomain = "SUSE-Public-Domain"
showLicense Fedora AllRightsReserved = "Proprietary"
showLicense SUSE AllRightsReserved = "SUSE-NonFree"
showLicense _ OtherLicense = "Unknown"
showLicense _ (UnknownLicense l) = "Unknown" +-+ l
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,16,0)
showLicense Fedora (Apache Nothing) = "ASL ?"
showLicense SUSE (Apache Nothing) = "Apache-2.0"
showLicense Fedora (Apache (Just ver)) = "ASL" +-+ showVersion ver
showLicense SUSE (Apache (Just ver)) = "Apache-" +-+ showVersion ver
#endif
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,18,0)
showLicense _ (AGPL Nothing) = "AGPLv?"
showLicense _ (AGPL (Just ver)) = "AGPLv" ++ showVersion ver
#endif

-- from http://stackoverflow.com/questions/930675/functional-paragraphs
-- using split would be: map unlines . (Data.List.Split.splitWhen null)
paragraphs :: [String] -> [String]
paragraphs = map (unlines . filter (not . null)) . groupBy (const $ not . null)

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

data Distro = Fedora | SUSE deriving (Eq)

-- for now assume Fedora if no /etc/SuSE-release
detectDistro :: IO Distro
detectDistro = do
  suseRelease <- doesFileExist $ "/etc" </> "SuSE-release"
  return $ if suseRelease then SUSE else Fedora
