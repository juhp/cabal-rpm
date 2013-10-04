{-# LANGUAGE CPP #-}

-- |
-- Module      :  Commands.Spec
-- Copyright   :  Bryan O'Sullivan 2007, 2008
--                Jens Petersen 2012-2013
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Generates an RPM spec file from a .cabal file.

-- This software may be used and distributed according to the terms of
-- the GNU General Public License, incorporated herein by reference.

module Commands.Spec (
  createSpecFile
  ) where

import PackageUtils (buildDependencies,
                     depName, packageName, packageVersion,
                     simplePackageDescription, showDep)
import Setup (RpmFlags (..))
import SysCmd ((+-+), tryReadProcess)

--import Control.Exception (bracket)
import Control.Monad    (unless, when)
import Data.Char        (toLower)
import Data.List        (groupBy, isPrefixOf, isSuffixOf, nub, sort)
import Data.Maybe       (fromMaybe)
import Data.Time.Clock  (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime)
import Data.Version     (showVersion)

import Distribution.License  (License (..))

import Distribution.Simple.Utils (warn)

import Distribution.PackageDescription (PackageDescription (..), exeName,
                                        hasExes, hasLibs, withExe, allBuildInfo,
                                        BuildInfo (..))

--import Distribution.Version (VersionRange, foldVersionRange')

import Distribution.PackageDescription (GenericPackageDescription (..))

import System.Directory (doesDirectoryExist, doesFileExist,
                         getDirectoryContents)
import System.IO     (IOMode (..), hClose, hPutStrLn, openFile)
import System.Locale (defaultTimeLocale)
import System.FilePath (dropFileName, takeDirectory, (</>))

import qualified Paths_cabal_rpm (version)


defaultRelease :: FilePath -> UTCTime -> IO String
defaultRelease cabalPath now = do
    let pkgDir = takeDirectory cabalPath
    darcsRepo <- doesDirectoryExist $ pkgDir </> "_darcs"
    gitRepo <- doesDirectoryExist $ pkgDir </> ".git"
    return $ if (darcsRepo || gitRepo)
               then formatTime defaultTimeLocale "0.%Y%m%d" now
               else "1"

rstrip :: (Char -> Bool) -> String -> String
rstrip p = reverse . dropWhile p . reverse

-- packageName :: PackageIdentifier -> String
-- packageName pkg = name
--   where PackageName name = pkgName pkg

-- packageVersion :: PackageIdentifier -> String
-- packageVersion pkg = (showVersion . pkgVersion) pkg

createSpecFile :: FilePath            -- ^pkg spec file
               -> GenericPackageDescription  -- ^pkg description
               -> RpmFlags            -- ^rpm flags
               -> IO ()
createSpecFile cabalPath genPkgDesc flags = do
    let verbose = rpmVerbosity flags
    now <- getCurrentTime
    defRelease <- defaultRelease cabalPath now
    pkgDesc <- simplePackageDescription genPkgDesc flags
    let pkg = package pkgDesc
        name = packageName pkg
        pkgname = if isExec then name else "ghc-" ++ name
        pkg_name = if isExec then "%{name}" else "%{pkg_name}"
        version = packageVersion pkg
        release = fromMaybe defRelease (rpmRelease flags)
        specFile = pkgname ++ ".spec"
        isExec = if (rpmLibrary flags) then False else hasExes pkgDesc
        isLib = hasLibs pkgDesc
        isBinLib = isExec && isLib
    specAlreadyExists <- doesFileExist specFile
    let specFilename = specFile ++ if (not $ rpmForce flags) && specAlreadyExists then ".cblrpm" else ""
    when specAlreadyExists $ putStrLn $ specFile +-+ "exists:" +-+ "creating" +-+ specFilename
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
    let syn' = if (null syn)
              then ("Haskell" +-+ name +-+ "package")
              else (unwords $ lines syn)
    let summary = rstrip (== '.') syn'
    when ((length $ "Summary     : " ++ syn') > 79) $
      warn verbose "this package has a long synopsis."

    let descr = description pkgDesc
    when (null descr) $
      warn verbose "this package has no description."
    let descLines = (formatParagraphs . lines . finalPeriod) $
          if (null descr) then syn' else descr
        finalPeriod cs = if (last cs == '.') then cs else cs ++ "."

    when isLib $ do
      putDef "pkg_name" name
      putNewline

    putHdr "Name" (if isExec then (if isLib then "%{pkg_name}" else pkgname) else "ghc-%{pkg_name}")
    putHdr "Version" version
    putHdr "Release" $ release ++ "%{?dist}"
    putHdr "Summary" summary
    putNewline
    putHdr "License" $ (showLicense . license) pkgDesc
    putHdr "URL" $ "http://hackage.haskell.org/package/" ++ pkg_name
    putHdr "Source0" $ "http://hackage.haskell.org/packages/archive/" ++ pkg_name ++ "/%{version}/" ++ pkg_name ++ "-%{version}.tar.gz"
    putNewline
    putHdr "BuildRequires" "ghc-Cabal-devel"
    putHdr "BuildRequires" $ "ghc-rpm-macros"

    let (deps, selfdep) = buildDependencies pkgDesc name
        buildinfo = allBuildInfo pkgDesc
        excludedTools n = notElem n ["ghc", "hsc2hs", "perl"]
        mapTools "gtk2hsC2hs" = "gtk2hs-buildtools"
        mapTools "gtk2hsHookGenerator" = "gtk2hs-buildtools"
        mapTools "gtk2hsTypeGen" = "gtk2hs-buildtools"
        mapTools tool = tool
        chrpath = if selfdep then ["chrpath"] else []
        tools = filter excludedTools $ nub $ (map (mapTools . depName) $ concat (map buildTools buildinfo)) ++ chrpath
        pkgcfgs = nub $ map depName $ concat (map pkgconfigDepends buildinfo)
        showPkgCfg p = "pkgconfig(" ++ p ++ ")"

    clibs <- mapM repoqueryLib $ concat (map extraLibs buildinfo)
    when (not . null $ deps ++ tools ++ clibs ++ pkgcfgs) $ do
      put "# Begin cabal-rpm deps:"
      let pkgdeps = sort $ map showDep deps ++ tools ++ map (++ "%{?_isa}") clibs ++ map showPkgCfg pkgcfgs
      mapM_ (putHdr "BuildRequires") pkgdeps
      when (any (\ d -> elem d ["template-haskell", "hamlet"]) deps) $
        putHdr "ExclusiveArch" "%{ghc_arches_with_ghci}"
      put "# End cabal-rpm deps"

    putNewline

    put "%description"
    mapM_ put descLines
    putNewline

    let wrapGenDesc = wordwrap (79 - max 0 (length pkgname - length pkg_name))

    when isLib $ do
      when isExec $ do
        put $ "%package" +-+ ghcPkg
        putHdr "Summary" $ "Haskell" +-+ pkg_name +-+ "library"
        putNewline
        put $ "%description" +-+ ghcPkg
        put $ wrapGenDesc $ "This package provides the Haskell" +-+ pkg_name +-+ "shared library."
        putNewline
      put $ "%package" +-+ ghcPkgDevel
      putHdr "Summary" $ "Haskell" +-+ pkg_name +-+ "library development files"
      putHdr "Provides" $ (if isExec then "ghc-%{name}" else "%{name}") ++ "-static = %{version}-%{release}"
      putHdr "Requires" "ghc-compiler = %{ghc_version}"
      putHdr "Requires(post)" "ghc-compiler = %{ghc_version}"
      putHdr "Requires(postun)" "ghc-compiler = %{ghc_version}"
      putHdr "Requires" $ (if isExec then "ghc-%{name}" else "%{name}") ++ "%{?_isa} = %{version}-%{release}"
      when (not . null $ clibs ++ pkgcfgs) $ do
        put "# Begin cabal-rpm deps:"
        mapM_ (putHdr "Requires") $ map (++ "%{?_isa}") clibs
        mapM_ (putHdr "Requires") $ map showPkgCfg pkgcfgs
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
    let pkgType = if isLib then "lib" else "bin"
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

    when isLib $ do
      put $ "%post" +-+ ghcPkgDevel
      put "%ghc_pkg_recache"
      putNewline
      putNewline
      put $ "%postun" +-+ ghcPkgDevel
      put "%ghc_pkg_recache"
      putNewline
      putNewline

    docs <- findDocs cabalPath pkgDesc

    when isExec $ do
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
      unless (null (dataFiles pkgDesc) && isExec) $
        put "%{_datadir}/%{name}-%{version}"

      putNewline
      putNewline

    when isLib $ do
      let baseFiles = if isBinLib then "-f ghc-%{name}.files" else "-f %{name}.files"
          develFiles = if isBinLib then "-f ghc-%{name}-devel.files" else "-f %{name}-devel.files"
      put $ "%files" +-+ ghcPkg +-+ baseFiles
      put $ "%doc" +-+ licenseFile pkgDesc
      putNewline
      putNewline
      put $ "%files" +-+ ghcPkgDevel +-+  develFiles
      unless (null docs) $
        put $ "%doc" +-+ unwords docs
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

-- from http://stackoverflow.com/questions/930675/functional-paragraphs
-- using split would be: map unlines . (Data.List.Split.splitWhen null)
paragraphs :: [String] -> [String]
paragraphs = map unlines . map (filter $ not . null) . groupBy (const $ not . null)

-- http://rosettacode.org/wiki/Word_wrap#Haskell
wordwrap :: Int -> String -> String
wordwrap maxlen = (wrap_ 0 False) . words where
	wrap_ _ _ [] = "\n"
	wrap_ pos eos (w:ws)
		-- at line start: put down the word no matter what
		| pos == 0 = w ++ wrap_ (pos + lw) endp ws
		| pos + lw + 1 > maxlen - 9 && eos = '\n':wrap_ 0 endp (w:ws)
		| pos + lw + 1 > maxlen = '\n':wrap_ 0 endp (w:ws)
		| otherwise = " " ++ w ++ wrap_ (pos + lw + 1) endp ws
		where lw = length w
                      endp = last w == '.'

formatParagraphs :: [String] -> [String]
formatParagraphs = map (wordwrap 79) . paragraphs

repoqueryLib :: String -> IO String
repoqueryLib lib = do
  let lib_path = "/usr/lib/lib" ++ lib ++ ".so"
  out <- tryReadProcess "repoquery" ["--qf=%{name}", "-qf", lib_path]
  let pkgs = nub $ words out
  case pkgs of
    [pkg] -> return pkg
    [] -> error $ "Could not resolve package that provides lib" ++ lib_path
    _ -> error $ "More than one package seems to provide lib" ++ lib_path ++ ": " ++ (show pkgs)
