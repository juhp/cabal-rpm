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

import Dependencies (packageDependencies, showDep)
import FileUtils (fileWithExtension)
import PackageUtils (isScmDir, packageName, packageVersion)
import Setup (RpmFlags (..))
import SysCmd ((+-+))

--import Control.Exception (bracket)
import Control.Monad    (unless, when)
import Data.Char        (toLower, toUpper)
import Data.List        (groupBy, isPrefixOf, isSuffixOf, sort)
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
import System.FilePath (dropFileName, takeBaseName, takeDirectory, (</>))

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
    let verbose = rpmVerbosity flags
    now <- getCurrentTime
    defRelease <- defaultRelease cabalPath now
    mspcfile <- if rpmForce flags then return Nothing
                else fileWithExtension "." ".spec"
    let mpkgname = fmap takeBaseName mspcfile
        pkg = package pkgDesc
        name = packageName pkg
        pkgname = fromMaybe (if hasExec then name else "ghc-" ++ name) mpkgname
        pkg_name = if pkgname == name then "%{name}" else "%{pkg_name}"
        basename | isBinLib = "%{pkg_name}"
                 | hasExecPkg = name
                 | otherwise = "ghc-%{pkg_name}"
        version = packageVersion pkg
        release = fromMaybe defRelease (rpmRelease flags)
        specFile = fromMaybe "" mdest </> pkgname ++ ".spec"
        hasExec = hasExes pkgDesc
        hasLib = hasLibs pkgDesc
        isBinLib = hasLib && not (rpmLibrary flags) && pkgname == name
        hasExecPkg = not (rpmLibrary flags) && (isBinLib || hasExes pkgDesc && not hasLib)
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
    let descLines = (formatParagraphs . initialCapital . filterSymbols . finalPeriod) $
          if null descr then syn' else descr
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

    let eCsources = concatMap (cSources . buildInfo) $ executables pkgDesc
    let lCsources = concatMap (cSources . libBuildInfo) $ maybeToList $ library pkgDesc
    when (null $ eCsources ++ lCsources) $ do
      put "# no useful debuginfo for Haskell packages without C sources"
      putDef "debug_package" "%{nil}"
      putNewline

    putHdr "Name" (if isBinLib then "%{pkg_name}" else basename)
    putHdr "Version" version
    putHdr "Release" $ release ++ "%{?dist}"
    putHdr "Summary" summary
    putNewline
    putHdr "License" $ (showLicense . license) pkgDesc
    putHdr "URL" $ "http://hackage.haskell.org/package/" ++ pkg_name
    putHdr "Source0" $ "http://hackage.haskell.org/package/" ++ pkg_name ++ "-%{version}/" ++ pkg_name ++ "-%{version}.tar.gz"
    putNewline
    putHdr "BuildRequires" "ghc-Cabal-devel"
    putHdr "BuildRequires" "ghc-rpm-macros"

    (deps, tools, clibs, pkgcfgs, selfdep) <- packageDependencies pkgDesc name
    let alldeps = sort $ deps ++ tools ++ map (++ "%{?_isa}") clibs ++ pkgcfgs
    unless (null alldeps) $ do
      put "# Begin cabal-rpm deps:"
      mapM_ (putHdr "BuildRequires") alldeps
      when (any (\ d -> d `elem` map showDep ["template-haskell", "hamlet"]) deps) $
        putHdr "ExclusiveArch" "%{ghc_arches_with_ghci}"
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
        putNewline
        put $ "%description" +-+ ghcPkg
        put $ wrapGenDesc $ "This package provides the Haskell" +-+ pkg_name +-+ "shared library."
        putNewline
      put $ "%package" +-+ ghcPkgDevel
      putHdr "Summary" $ "Haskell" +-+ pkg_name +-+ "library development files"
      putHdr "Provides" $ (if isBinLib then "ghc-%{name}" else "%{name}") ++ "-static = %{version}-%{release}"
      putHdr "Requires" "ghc-compiler = %{ghc_version}"
      putHdr "Requires(post)" "ghc-compiler = %{ghc_version}"
      putHdr "Requires(postun)" "ghc-compiler = %{ghc_version}"
      putHdr "Requires" $ (if isBinLib then "ghc-%{name}" else "%{name}") ++ "%{?_isa} = %{version}-%{release}"
      unless (null $ clibs ++ pkgcfgs) $ do
        put "# Begin cabal-rpm deps:"
        mapM_ (putHdr "Requires") $ sort $ map (++ "%{?_isa}") clibs ++ pkgcfgs
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

    when hasLib $ do
      let putInstallScript = do
            put "%ghc_pkg_recache"
            putNewline
            putNewline
      put $ "%post" +-+ ghcPkgDevel
      putInstallScript
      put $ "%postun" +-+ ghcPkgDevel
      putInstallScript

    docs <- findDocs cabalPath pkgDesc

    when hasExecPkg $ do
      put "%files"
      -- Add the license file to the main package only if it wouldn't
      -- otherwise be empty.
      unless (null $ licenseFile pkgDesc) $
        put $ "%doc" +-+ licenseFile pkgDesc
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
      put $ "%doc" +-+ licenseFile pkgDesc
      -- be strict for now
--      unless (null (dataFiles pkgDesc) || isBinLib) $
--        put "%{_datadir}/%{pkg_name}-%{version}"
      putNewline
      putNewline
      put $ "%files" +-+ ghcPkgDevel +-+  develFiles
      unless (null docs) $
        put $ "%doc" +-+ unwords docs
      when (not isBinLib && hasExec) $
        withExe pkgDesc $ \exe ->
        let program = exeName exe in
        put $ "%{_bindir}/" ++ (if program == name then "%{name}" else program)
      putNewline
      putNewline

    let date = formatTime defaultTimeLocale "%a %b %e %Y" now
    put "%changelog"
    put $ "*" +-+ date +-+ "Fedora Haskell SIG <haskell@lists.fedoraproject.org> - " ++ version
    put $ "- spec file generated by cabal-rpm-" ++ showVersion Paths_cabal_rpm.version
    hClose h

findDocs :: FilePath -> PackageDescription -> IO [FilePath]
findDocs cabalPath pkgDesc = do
    contents <- getDirectoryContents $ dropFileName cabalPath
    let docs = filter likely contents
    return $ if null lf
             then docs
             else filter unlikely $ filter (/= lf) docs
  where names = ["author", "copying", "doc", "example", "licence", "license",
                 "readme", "todo"]
        likely name = let lowerName = map toLower name
                      in any (`isPrefixOf` lowerName) names
        lf = licenseFile pkgDesc
        unlikely name = not $ any (`isSuffixOf` name) ["~"]

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
showLicense (UnknownLicense l) = "Unknown" +-+ l
#if MIN_VERSION_Cabal(1,16,0)
showLicense (Apache Nothing) = "ASL ?"
showLicense (Apache (Just ver)) = "ASL" +-+ showVersion ver
#endif
#if MIN_VERSION_Cabal(1,18,0)
showLicense (AGPL Nothing) = "AGPLv?"
showLicense (AGPL (Just ver)) = "AGPLv" ++ showVersion ver
#endif

-- from http://stackoverflow.com/questions/930675/functional-paragraphs
-- using split would be: map unlines . (Data.List.Split.splitWhen null)
paragraphs :: [String] -> [String]
paragraphs = map (unlines . filter (not . null)) . groupBy (const $ not . null)

-- http://rosettacode.org/wiki/Word_wrap#Haskell
wordwrap :: Int -> String -> String
wordwrap maxlen = wrap_ 0 False . words where
	wrap_ _ _ [] = "\n"
	wrap_ pos eos (w:ws)
		-- at line start: put down the word no matter what
		| pos == 0 = w ++ wrap_ (pos + lw) endp ws
		| pos + lw + 1 > maxlen - 9 && eos = '\n':wrap_ 0 endp (w:ws)
		| pos + lw + 1 > maxlen = '\n':wrap_ 0 endp (w:ws)
		| otherwise = " " ++ w ++ wrap_ (pos + lw + 1) endp ws
		where lw = length w
                      endp = last w == '.'

formatParagraphs :: String -> [String]
formatParagraphs = map (wordwrap 79) . paragraphs . lines
