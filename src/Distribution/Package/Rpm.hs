-- |
-- Module      :  Distribution.Package.Rpm
-- Copyright   :  Bryan O'Sullivan 2007, 2008
--                Jens Petersen 2012
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Support for building RPM packages.  Can also generate
-- an RPM spec file if you need a basic one to hand-customize.

-- This software may be used and distributed according to the terms of
-- the GNU General Public License, incorporated herein by reference.

module Distribution.Package.Rpm (
      createSpecFile
    , rpmBuild
    ) where

--import Control.Exception (bracket)
import Control.Monad    (unless, void, when)
import Data.Char        (toLower)
import Data.List        (intercalate, isPrefixOf, isSuffixOf, nub, sort)
import Data.Maybe       (fromMaybe)
import Data.Time.Clock  (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime)
import Data.Version     (showVersion)

import Distribution.License  (License (..))
import Distribution.Package  (Dependency (..), PackageIdentifier (..),
                              PackageName (..))

import Distribution.Simple.Utils (die, warn)

import Distribution.PackageDescription (PackageDescription (..), exeName,
                                        hasExes, hasLibs, withExe, allBuildInfo,
                                        BuildInfo (..))

import Distribution.Verbosity (normal)
--import Distribution.Version (VersionRange, foldVersionRange')

import Distribution.Package.Rpm.Setup (RpmFlags (..))

import System.Cmd (system)
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)
import System.Exit (ExitCode(..))
import System.Environment (getEnv)
import System.IO     (IOMode (..), hClose, hPutStrLn, openFile)
import System.Locale (defaultTimeLocale)
--import System.Process (runInteractiveCommand, waitForProcess)
import System.FilePath (dropFileName)
import System.FilePath.Posix ((</>))

import qualified Paths_cabal_rpm (version)

(+-+) :: String -> String -> String
"" +-+ s = s
s +-+ "" = s
s +-+ t = s ++ " " ++ t

-- autoreconf :: Verbosity -> PackageDescription -> IO ()
-- autoreconf verbose pkgDesc = do
--     ac <- doesFileExist "configure.ac"
--     when ac $ do
--         c <- doesFileExist "configure"
--         when (not c) $ do
--             setupMessage verbose "Running autoreconf" pkgDesc
--             trySystem "autoreconf"

unlessSystem :: String -> String -> IO ()
unlessSystem tst act = do
    ret <- system tst
    case ret of
      ExitSuccess -> return ()
      ExitFailure _ -> void $ trySystemWarn act

maybeInstall :: String -> IO ()
maybeInstall pkg = do
    unlessSystem ("rpm -q" +-+ pkg) ("sudo yum install" +-+ pkg)

rpmBuild :: FilePath -> PackageDescription -> RpmFlags -> Bool -> IO ()
rpmBuild cabalPath pkgDesc flags binary = do
--    let verbose = rpmVerbosity flags
--    flip mapM_ ["BUILD", "RPMS", "SOURCES", "SPECS", "SRPMS"] $ \ subDir -> do
--      createDirectoryIfMissing True (tgtPfx </> subDir)
--    bracket (setFileCreationMask 0o022) setFileCreationMask $ \ _ -> do
--      autoreconf verbose pkgDesc
    specFile <- specFileName pkgDesc flags
    specFileExists <- doesFileExist specFile
    unless specFileExists $
      createSpecFile cabalPath pkgDesc flags
    let pkg = package pkgDesc
        name = packageName pkg
    when binary $ do
        --      trySystem ("sudo yum-builddep" +-+ specFile)
        mapM_ maybeInstall $ map showDep $ buildDependencies pkgDesc [name]
    cwd <- getCurrentDirectory
    home <- getEnv "HOME"
    let version = packageVersion pkg
        cachedir = home </> ".cabal/packages/hackage.haskell.org" </> name </> version
        tarFile = name ++ "-" ++ version ++ ".tar.gz"
        rpmCmd = if binary then "a" else "s"
    tarFileExists <- doesFileExist tarFile
    srcdir <- if tarFileExists
                then return cwd
                else do
                     trySystem ("cabal fetch -v0 --no-dependencies" +-+ name ++ "-" ++ version)
                     return cachedir
    trySystem ("rpmbuild -b" ++ rpmCmd +-+
                     "--define \"_rpmdir" +-+ cwd ++ "\"" +-+
                     "--define \"_srcrpmdir" +-+ cwd ++ "\"" +-+
                     "--define \"_sourcedir" +-+ srcdir ++ "\"" +-+
                     specFile)

trySystem :: String -> IO ()
trySystem cmd = do
    ret <- system cmd
    case ret of
      ExitSuccess -> return ()
      ExitFailure n -> die ("\"" ++ cmd ++ "\"" +-+ "failed with status" +-+ show n)

trySystemWarn :: String -> IO ()
trySystemWarn cmd = do
    ret <- system cmd
    case ret of
      ExitSuccess -> return ()
      ExitFailure n -> warn normal ("\"" ++ cmd ++ "\"" +-+ "failed with status" +-+ show n)

defaultRelease :: UTCTime -> IO String
defaultRelease now = do
    darcsRepo <- doesDirectoryExist "_darcs"
    return $ if darcsRepo
               then formatTime defaultTimeLocale "0.%Y%m%d" now
               else "1"

rstrip :: (Char -> Bool) -> String -> String
rstrip p = reverse . dropWhile p . reverse

packageName :: PackageIdentifier -> String
packageName pkg = name
  where PackageName name = pkgName pkg

packageVersion :: PackageIdentifier -> String
packageVersion pkg = (showVersion . pkgVersion) pkg

specFileName :: PackageDescription    -- ^pkg description
               -> RpmFlags            -- ^rpm flags
               -> IO FilePath
specFileName pkgDesc flags = do
    let pkg = package pkgDesc
        name = packageName pkg
        pkgname = if isExec then name else "ghc-" ++ name
        isExec = if (rpmLibrary flags) then False else hasExes pkgDesc
    return $ pkgname ++ ".spec"

buildDependencies :: PackageDescription -> [String] -> [String]
buildDependencies pkgDesc excl = filter excludedPkgs $ nub $ map depName (buildDepends pkgDesc)
  where excludedPkgs n = notElem n $ ["ghc-prim", "integer-gmp"] ++ excl

depName :: Dependency -> String
depName (Dependency (PackageName n) _) = n

showDep :: String -> String
showDep p = "ghc-" ++ p ++ "-devel"

createSpecFile :: FilePath            -- ^pkg spec file
               -> PackageDescription  -- ^pkg description
               -> RpmFlags            -- ^rpm flags
               -> IO ()
createSpecFile cabalPath pkgDesc flags = do
    let verbose = rpmVerbosity flags
    now <- getCurrentTime
    defRelease <- defaultRelease now
    let pkg = package pkgDesc
        name = packageName pkg
        pkgname = if isExec then name else "ghc-" ++ name
        pkg_name = if isExec then "%{name}" else "%{pkg_name}"
        version = packageVersion pkg
        release = fromMaybe defRelease (rpmRelease flags)
        specFile = pkgname ++ ".spec"
        isExec = if (rpmLibrary flags) then False else hasExes pkgDesc
        isLib = hasLibs pkgDesc
    specAlreadyExists <- doesFileExist specFile
    h <- openFile (specFile ++ if specAlreadyExists then ".cblrpm" else "") WriteMode
    let putHdr hdr val = hPutStrLn h (hdr ++ ":" ++ padding hdr ++ val)
        padding hdr = replicate (15 - length hdr) ' '
        putHdr_ hdr val = unless (null val) $ putHdr hdr val
        putHdrD hdr val dfl = putHdr hdr (if null val then dfl else val)
        putNewline = hPutStrLn h ""
        put = hPutStrLn h
        putDef v s = put $ "%global" +-+ v +-+ s
        date = formatTime defaultTimeLocale "%a %b %e %Y" now

    put "# https://fedoraproject.org/wiki/Packaging:Haskell"
    put "# https://fedoraproject.org/wiki/PackagingDrafts/Haskell"
    putNewline

    -- Some packages conflate the synopsis and description fields.  Ugh.
    let syn = synopsis pkgDesc
    (syn', synTooLong) <- case lines syn of
              (x:_) -> return (x, x /= syn)
              _ -> do warn verbose "This package has no synopsis."
                      return ("Haskell" +-+ name +-+ "package", False)
    let common_summary = if synTooLong
                         then syn' +-+ "[...]"
                         else rstrip (== '.') syn'
    when synTooLong $
      warn verbose "The synopsis for this package spans multiple lines."

    let common_description = lines $
          if (null . description) pkgDesc
              then if synTooLong
                   then syn
                   else "This package does not have a description."
              else description pkgDesc

    when isLib $ do
      putDef "pkg_name" name
      putNewline
      putDef "common_summary" common_summary
      putNewline
      putDef "common_description" $ intercalate "\\\n" common_description
      putNewline

    putHdr "Name" (if isExec then (if isLib then "%{pkg_name}" else pkgname) else "ghc-%{pkg_name}")
    putHdr "Version" version
    putHdr "Release" $ release ++ "%{?dist}"
    if isLib
      then putHdr "Summary" "%{common_summary}"
      else putHdrD "Summary" common_summary "This package has no summary"
    putNewline
    putHdr "License" $ (showLicense . license) pkgDesc
    putHdr_ "URL" $ "http://hackage.haskell.org/package/" ++ pkg_name
    putHdr "Source0" $ "http://hackage.haskell.org/packages/archive/" ++ pkg_name ++ "/%{version}/" ++ pkg_name ++ "-%{version}.tar.gz"
    putNewline
    putHdr "BuildRequires" "ghc-Cabal-devel"
    putHdr "BuildRequires" $ "ghc-rpm-macros"

    let deps = buildDependencies pkgDesc [name, "Cabal", "base"]
        buildinfo = allBuildInfo pkgDesc
        excludedTools n = notElem n ["ghc", "perl"]
        mapTools "gtk2hsC2hs" = "gtk2hs-buildtools"
        mapTools "gtk2hsHookGenerator" = "gtk2hs-buildtools"
        mapTools "gtk2hsTypeGen" = "gtk2hs-buildtools"
        mapTools tool = tool
        tools = filter excludedTools $ nub $ map (mapTools . depName) $ concat (map buildTools buildinfo)
        excludedCLibs n = notElem n []
        mapCLibs "glut" = "freeglut"
        mapCLibs "iw" = "wireless-tools"
        mapCLibs "z" = "zlib"
        mapCLibs ('X':lib) = "libX" ++ lib
        mapCLibs lib = lib
        clibs = sort $ filter excludedCLibs $ nub $ map mapCLibs $ concat (map extraLibs buildinfo)
        pkgcfgs = nub $ map depName $ concat (map pkgconfigDepends buildinfo)
        showPkgCfg p = "pkgconfig(" ++ p ++ ")"

    when (not . null $ deps ++ tools ++ clibs ++ pkgcfgs) $ do
      put "# Begin cabal-rpm deps:"
      mapM_ (putHdr "BuildRequires") $ map showDep deps
      when (any (\ d -> elem d ["template-haskell", "hamlet"]) deps) $
        putHdr "ExclusiveArch" "%{ghc_arches_with_ghci}"
      mapM_ (putHdr "BuildRequires") tools
      mapM_ (putHdr "BuildRequires") $ map (++ "-devel%{?_isa}") clibs
      mapM_ (putHdr "BuildRequires") $ map showPkgCfg pkgcfgs
      put "# End cabal-rpm deps"

    putNewline

    put "%description"
    put $ if isLib then "%{common_description}" else unlines common_description
    putNewline
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
    putNewline
    putNewline

    when (isExec && isLib) $ do
      put "%ghc_package"
      putNewline
      put "%ghc_description"
      putNewline
      putNewline

    when isLib $ do
      put "%ghc_devel_package"
      when (not . null $ clibs ++ pkgcfgs) $ do
        put "# Begin cabal-rpm deps:"
        mapM_ (putHdr "Requires") $ map (++ "-devel%{?_isa}") clibs
        mapM_ (putHdr "Requires") $ map showPkgCfg pkgcfgs
        put "# End cabal-rpm deps"
      putNewline
      put "%ghc_devel_description"
      putNewline
      putNewline
      put "%ghc_devel_post_postun"
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
      put $ "%ghc_files" +-+ licenseFile pkgDesc
      unless (null docs) $
        put $ "%doc" +-+ unwords docs
      putNewline
      putNewline

    put "%changelog"
    put $ "*" +-+ date +-+ "Fedora Haskell SIG <haskell@lists.fedoraproject.org>"
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
