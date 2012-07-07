-- |
-- Module      :  Distribution.Package.Rpm
-- Copyright   :  Bryan O'Sullivan 2007, 2008
--
-- Maintainer  :  Bryan O'Sullivan <bos@serpentine.com>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Support for building RPM packages.  Can also generate
-- an RPM spec file if you need a basic one to hand-customize.

-- This software may be used and distributed according to the terms of
-- the GNU General Public License, incorporated herein by reference.

module Distribution.Package.Rpm (
      createSpecFile
    , rpm
--    , rpmBuild
    ) where

--import Control.Exception (bracket)
import Control.Monad (when) -- unless
import Data.Char (toLower)
import Data.List (intersperse, isPrefixOf, sort)
--import Data.Maybe
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime)
import Data.Version (showVersion)
--import System.Cmd (system)
import System.Directory (doesDirectoryExist, doesFileExist,
                         getDirectoryContents)
--import System.Exit (ExitCode(..))
import System.IO (IOMode(..), hClose, hPutStrLn, openFile)
import System.Locale (defaultTimeLocale)
--import System.Process (runInteractiveCommand, waitForProcess)

import System.FilePath ((</>))
import Distribution.Simple.Compiler (CompilerFlavor(..), Compiler(..))
import Distribution.System (Platform(..), buildOS, buildArch)
import Distribution.License (License(..))
import Distribution.Package (PackageIdentifier(..), PackageName(..))
--import Distribution.Simple.PreProcess (knownSuffixHandlers)
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Configure (configCompiler)
--import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
--import Distribution.Simple.SrcDist (createArchive, prepareTree)
import Distribution.Simple.Utils (die, warn)
import Distribution.Package (Dependency(..))
import Distribution.PackageDescription (-- BuildInfo(..),
                                        GenericPackageDescription(..),
                                        PackageDescription(..),
                                        exeName,
                                        hasExes,
                                        hasLibs,
                                        withExe,
--                                        withLib
                                       )
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.Verbosity (Verbosity)
import Distribution.Version (VersionRange(..))
--import Distribution.Simple.Setup (configConfigurationsFlags, emptyConfigFlags)
--import Distribution.Package.Rpm.Bundled (bundledWith, isBundled)
import Distribution.Package.Rpm.Setup (RpmFlags(..))
--import System.Posix.Files (setFileCreationMask)

commaSep :: [String] -> String
commaSep = concat . intersperse ", "

simplePackageDescription :: GenericPackageDescription -> RpmFlags
                         -> IO (Compiler, PackageDescription)
simplePackageDescription genPkgDesc flags = do
    (compiler, _) <- configCompiler (rpmCompiler flags) Nothing Nothing
                     defaultProgramConfiguration
                     (rpmVerbosity flags)
    case finalizePackageDescription (rpmConfigurationsFlags flags)
          (const True) (Platform buildArch buildOS) (compilerId compiler)
          {- (Nothing :: Maybe PackageIndex) -}
          [] genPkgDesc of
      Left e -> die $ "finalize failed: " ++ show e
      Right (pd, _) -> return (compiler, pd)

rpm :: GenericPackageDescription -- ^info from the .cabal file
    -> RpmFlags                 -- ^rpm flags
    -> IO ()

rpm genPkgDesc flags = do
    let comp = rpmCompiler flags
    case comp of
      Just GHC -> return ()
      Just c -> die ("the " ++ show c ++ " compiler is not yet supported")
      _ -> die "no compiler information provided"
    (_compiler, pkgDesc) <- simplePackageDescription genPkgDesc flags
    (_, extraDocs) <- createSpecFile pkgDesc flags "."
    when ((not . null) extraDocs) $ do
      putStrLn "Docs not in .cabal packaged:"
      mapM_ putStrLn $ sort extraDocs
    return ()

-- | Copy a file or directory (recursively, in the latter case) to the
-- same name in the target directory.  Arguments flipped from the
-- conventional order.

-- copyTo :: Verbosity -> FilePath -> FilePath -> IO ()

-- copyTo verbose dest src = do
--     isFile <- doesFileExist src
--     let destDir = dest </> src
--     if isFile
--       then copyFileVerbose verbose src destDir
--       else copyDirectoryRecursiveVerbose verbose src destDir

-- autoreconf :: Verbosity -> PackageDescription -> IO ()

-- autoreconf verbose pkgDesc = do
--     ac <- doesFileExist "configure.ac"
--     when ac $ do
--         c <- doesFileExist "configure"
--         when (not c) $ do
--             setupMessage verbose "Running autoreconf" pkgDesc
--             ret <- system "autoreconf"
--             case ret of
--               ExitSuccess -> return ()
--               ExitFailure n -> die ("autoreconf failed with status " ++ show n)

-- localBuildInfo :: PackageDescription -> RpmFlags -> IO LocalBuildInfo
-- localBuildInfo pkgDesc flags = do
--   mb_lbi <- maybeGetPersistBuildConfig
--   case mb_lbi of
--     Just lbi -> return lbi
--     Nothing -> configure (Right pkgDesc, emptyHookedBuildInfo)
--                ((emptyConfigFlags defaultProgramConfiguration)
--                 { configConfigurationsFlags = rpmConfigurationsFlags flags })

-- rpmBuild :: GenericPackageDescription -> RpmFlags -> IO ()

-- rpmBuild genPkgDesc flags = do
--     tgtPfx <- canonicalizePath (rpmTopDir flags)
--     (compiler, pkgDesc) <- simplePackageDescription genPkgDesc flags
--     let verbose = rpmVerbosity flags
--         tmpDir = tgtPfx </> "src"
--     flip mapM_ ["BUILD", "RPMS", "SOURCES", "SPECS", "SRPMS"] $ \ subDir -> do
--       createDirectoryIfMissing True (tgtPfx </> subDir)
--     let specsDir = tgtPfx </> "SPECS"
--     lbi <- localBuildInfo pkgDesc flags
--     bracket (setFileCreationMask 0o022) setFileCreationMask $ \ _ -> do
-- --      autoreconf verbose pkgDesc
--       (specFile, extraDocs) <- createSpecFile pkgDesc flags compiler
--                                specsDir
--       tree <- prepareTree pkgDesc verbose (Just lbi) False tmpDir
--               knownSuffixHandlers 0
--       mapM_ (copyTo verbose tree) extraDocs
--       createArchive pkgDesc verbose (Just lbi) tmpDir (tgtPfx </> "SOURCES")
--       ret <- system ("rpmbuild -ba --define \"_topdir " ++ tgtPfx ++ "\" " ++
--                      specFile)
--       case ret of
--         ExitSuccess -> return ()
--         ExitFailure n -> die ("rpmbuild failed with status " ++ show n)

defaultRelease :: UTCTime -> IO String
defaultRelease now = do
    darcsRepo <- doesDirectoryExist "_darcs"
    return $ if darcsRepo
               then formatTime defaultTimeLocale "0.%Y%m%d" now
               else "1"

rstrip :: (Char -> Bool) -> String -> String
rstrip p = reverse . dropWhile p . reverse

createSpecFile :: PackageDescription  -- ^info from the .cabal file
               -> RpmFlags            -- ^rpm flags
               -> FilePath            -- ^directory in which to create file
               -> IO (FilePath, [FilePath])
createSpecFile pkgDesc flags tgtPfx = do
    now <- getCurrentTime
    defRelease <- defaultRelease now
    let pkg = package pkgDesc
        verbose = rpmVerbosity flags
        PackageName origName = pkgName pkg
        name = maybe (if isExec then origName else "ghc-" ++ origName) id (rpmName flags)
        pkg_name = (if isExec then "%{name}" else "%{pkg_name}")
        version = maybe ((showVersion . pkgVersion) pkg) id (rpmVersion flags)
        release = maybe defRelease id (rpmRelease flags)
        specPath = tgtPfx </> name ++ ".spec"
        isExec = hasExes pkgDesc
    specAlreadyExists <- doesFileExist specPath
    h <- openFile (specPath ++ if specAlreadyExists then ".cabal-rpm" else "") WriteMode
    let putHdr hdr val = hPutStrLn h (hdr ++ ":" ++ (padding hdr) ++ val)
        padding hdr = replicate (15 - (length hdr)) ' '
        putHdr_ hdr val = when (not $ null val) $
                              putHdr hdr val
        putHdrD hdr val dfl = putHdr hdr (if null val then dfl else val)
        putNewline = hPutStrLn h ""
        put s = hPutStrLn h s
        putDef v s = put $ "%global " ++ v ++ ' ' : s
        date = formatTime defaultTimeLocale "%a %b %d %Y" now

    when (hasLibs pkgDesc) $ do
      putDef "pkg_name" origName
      putNewline

    putHdr "Name" (if (name /= origName) then "ghc-%{pkg_name}" else name)
    putHdr "Version" version
    putHdr "Release" $ release ++ "%{?dist}"
    -- Some packages conflate the synopsis and description fields.  Ugh.
    let syn = synopsis pkgDesc
    (syn', synTooLong) <- case lines syn of
              (x:_) -> return (x, x /= syn)
              _ -> do warn verbose "This package has no synopsis."
                      return ("This package has no synopsis.", False)
    let summary = if synTooLong
                  then syn' ++ " [...]"
                  else rstrip (== '.') syn'
    when synTooLong $
        warn verbose "The synopsis for this package spans multiple lines."
    putHdrD "Summary" summary "This package has no summary"
    putNewline
    putHdr "License" $ (showLicense . license) pkgDesc
    putHdr_ "URL" $ "http://hackage.haskell.org/package/" ++ pkg_name
    putHdr "Source0" $ "http://hackage.haskell.org/packages/archive/" ++ pkg_name ++ "/%{version}/" ++ pkg_name ++ "-%{version}.tar.gz"
    putNewline
    putHdr "BuildRequires" "ghc-Cabal-devel"
    putHdr "BuildRequires" $ "ghc-rpm-macros" ++ (if (hasLibs pkgDesc) then " %{!?without_hscolour:hscolour}" else "")

    let  extDeps = (buildDepends pkgDesc)
    externalDeps <- mapM (showRpmReq verbose) extDeps
    mapM_ (putHdr "BuildRequires") $ map commaSep externalDeps
    -- External libraries incur both build-time and runtime
    -- dependencies.  The latter only need to be made explicit for the
    -- built library, as RPM is smart enough to ferret out good
    -- dependencies for binaries.
--    extDeps <- findLibDeps $ libBuildInfo pkgDesc
--    let extraReq = commaSep extDeps
--    putHdr_ "BuildRequires" extraReq
--    unless isExec $ do
--      putHdr_ "Requires" extraReq

    putNewline

    let putDesc = do
        put $ if (null . description) pkgDesc
              then if synTooLong
                   then syn
                   else "This package does not have a description."
              else description pkgDesc
    put "%description"
    putDesc
    putNewline
    putNewline

    {- Compiler-specific library data goes into a package of its own.

       Unlike a library for a traditional language, the library
       package depends on the compiler, because when installed, it
       has to register itself with the compiler's own package
       management system. -}

    when (isExec && hasLibs pkgDesc) $ do
        put "%package -n %{hsc_name}-%{lc_name}"
        putHdrD "Summary" summary "This library package has no summary"
        putNewline

        put "%description -n %{hsc_name}-%{lc_name}"
        putDesc
        putNewline
        putNewline

    put "%prep"
    put $ "%setup -q" ++ (if (name /= origName) then " -n %{pkg_name}-%{version}" else "")
    putNewline
    putNewline

    put "%build"
    let buildType = if (hasLibs pkgDesc) then "lib" else "bin"
    put $ "%ghc_" ++ buildType ++ "_build"
    putNewline
    putNewline

    put "%install"
    put $ "%ghc_" ++ buildType ++ "_install"
    putNewline
    putNewline

    when (hasLibs pkgDesc) $ do
      put "%ghc_devel_package"
      putNewline
      put "%ghc_devel_description"
      putNewline
      putNewline
      put "%ghc_devel_post_postun"
      putNewline
      putNewline

    when isExec $ do
      put "%files"
      -- Add the license file to the main package only if it wouldn't
      -- otherwise be empty.
      when ((not . null . licenseFile) pkgDesc ||
           (not . null . dataFiles) pkgDesc) $
        put $ "%doc " ++ licenseFile pkgDesc

      withExe pkgDesc $ \exe ->
        let program = exeName exe in
        put $ "%{_bindir}/" ++ (if (program == origName) then "%{name}" else program)
      when (((not . null . dataFiles) pkgDesc) && isExec) $
        put "%{_datadir}/%{name}-%{version}"

      putNewline
      putNewline

    docs <- findDocs pkgDesc
    when (hasLibs pkgDesc) $ do
      put $ "%ghc_files " ++ licenseFile pkgDesc
      when ((not . null) docs) $
        put $ "%doc " ++ concat (intersperse " " docs)
      putNewline
      putNewline

    put "%changelog"
    put ("* " ++ date ++ " cabal-rpm <cabal-devel@haskell.org> - " ++
         version ++ "-" ++ release)
    put "- spec file autogenerated by cabal-rpm"
    hClose h
    return (specPath, filter (`notElem` (extraSrcFiles pkgDesc)) docs)

findDocs :: PackageDescription -> IO [FilePath]

findDocs pkgDesc = do
    contents <- getDirectoryContents "."
    let docs = filter likely contents
    return $ if (null . licenseFile) pkgDesc
             then docs
             else let lf = licenseFile pkgDesc
                  in filter (/= lf) docs
  where names = ["author", "copying", "doc", "example", "licence", "license",
                 "readme", "todo"]
        likely name = let lowerName = map toLower name
                      in any (`isPrefixOf` lowerName) names

showLicense :: License -> String
showLicense (GPL Nothing) = "GPL+"
showLicense (GPL (Just ver)) = "GPLv" ++ (show ver) ++ "+"
showLicense (LGPL Nothing) = "LGPLv2+"
showLicense (LGPL (Just ver)) = "LGPLv" ++ (show ver) ++ "+"
showLicense BSD3 = "BSD"
showLicense BSD4 = "BSD"
showLicense MIT = "MIT"
showLicense PublicDomain = "Public Domain"
showLicense AllRightsReserved = "Proprietary"
showLicense OtherLicense = "Unknown"
showLicense (UnknownLicense l) = "Unknown " ++ l

-- | Generate a string expressing runtime dependencies, but only
-- on package/version pairs not already "built into" a compiler
-- distribution.

-- showRuntimeReq :: Verbosity -> PackageDescription -> IO String

-- showRuntimeReq verbose pkgDesc = do
--     let externalDeps = (buildDepends pkgDesc)
--     clauses <- mapM (showRpmReq verbose) externalDeps
--     return $ (commaSep . concat) clauses

ghc_devel :: String -> String
ghc_devel pkg = "ghc-" ++ pkg ++ "-devel"

-- | Represent a dependency in a form suitable for an RPM spec file.

showRpmReq :: Verbosity -> Dependency -> IO [String]

showRpmReq _ (Dependency (PackageName pkg) AnyVersion) =
    return [ghc_devel pkg]
showRpmReq _ (Dependency (PackageName pkg) (ThisVersion v)) =
    return [ghc_devel pkg ++ " = " ++ showVersion v]
showRpmReq _ (Dependency (PackageName pkg) (EarlierVersion v)) =
    return [ghc_devel pkg ++ " < " ++ showVersion v]
showRpmReq _ (Dependency (PackageName pkg) (LaterVersion v)) =
    return [ghc_devel pkg ++ " > " ++ showVersion v]
showRpmReq _ (Dependency (PackageName pkg) (UnionVersionRanges
                         (ThisVersion v1)
                         (LaterVersion v2)))
    | v1 == v2 = return [ghc_devel pkg ++ " >= " ++ showVersion v1]
showRpmReq _ (Dependency (PackageName pkg) (UnionVersionRanges
                         (ThisVersion v1)
                         (EarlierVersion v2)))
    | v1 == v2 = return [ghc_devel pkg ++ " <= " ++ showVersion v1]
showRpmReq verbose (Dependency (PackageName pkg) (UnionVersionRanges _ _)) = do
    warn verbose ("Cannot accurately represent " ++
                  "dependency on package " ++ pkg)
    warn verbose "  (uses version union, which RPM can't handle)"
    return [ghc_devel pkg]
showRpmReq verbose (Dependency (PackageName pkg) (IntersectVersionRanges r1 r2)) = do
    d1 <- showRpmReq verbose (Dependency (PackageName pkg) r1)
    d2 <- showRpmReq verbose (Dependency (PackageName pkg) r2)
    return (d1 ++ d2)

-- -- | Find the paths to all "extra" libraries specified in the package
-- -- config.  Prefer shared libraries, since that's what gcc prefers.
-- findLibPaths :: BuildInfo -> IO [FilePath]

-- findLibPaths buildInfo = mapM findLib (extraLibs buildInfo)
--   where findLib :: String -> IO FilePath
--         findLib lib = do
--             so <- findLibPath ("lib" ++ lib ++ ".so")
--             if isJust so
--               then return (fromJust so)
--               else findLibPath ("lib" ++ lib ++ ".a") >>=
--                    maybe (die $ "could not find library: lib" ++ lib)
--                          return
--         findLibPath extraLib = do
--             loc <- findInExtraLibs (extraLibDirs buildInfo)
--             if isJust loc
--               then return loc
--               else findWithGcc extraLib
--           where findInExtraLibs (d:ds) = do
--                     let path = d </> extraLib
--                     exists <- doesFileExist path
--                     if exists
--                       then return (Just path)
--                       else findInExtraLibs ds
--                 findInExtraLibs [] = return Nothing

-- | Return the full path to a file (usually an object file) that gcc
-- knows about.

-- findWithGcc :: FilePath -> IO (Maybe FilePath)

-- findWithGcc lib = do
--     (i,o,e,p) <- runInteractiveCommand $ "gcc -print-file-name=" ++ lib
--     loc <- hGetLine o
--     mapM_ hClose [i,o,e]
--     waitForProcess p
--     return $ if loc == lib then Nothing else Just loc

-- | Return the RPM that owns a particular file or directory.  Die if
-- not owned.

-- findRpmOwner :: FilePath -> IO String
-- findRpmOwner path = do
--     (i,o,e,p) <- runInteractiveCommand (rpmQuery ++ path)
--     pkg <- hGetLine o
--     mapM_ hClose [i,o,e]
--     ret <- waitForProcess p
--     case ret of
--       ExitSuccess -> return pkg
--       _ -> die $ "not owned by any package: " ++ path
--   where rpmQuery = "rpm --queryformat='%{NAME}' -qf "

-- | Find all RPMs on which the build of this package depends.  Die if
-- a dependency is not present, or not owned by an RPM.

--findLibDeps :: BuildInfo -> IO [String]

--findLibDeps buildInfo = findLibPaths buildInfo >>= mapM findRpmOwner
