{-# LANGUAGE CPP #-}

-- |
-- Module      :  Distribution.Package.Rpm
-- Copyright   :  Bryan O'Sullivan 2007
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
    , rpmBuild
    ) where

import Control.Exception (bracket)
import Control.Monad (filterM, liftM, mapM, when, unless)
import Data.Char (toLower)
import Data.List (find, intersperse, isPrefixOf, sort)
import Data.Maybe
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime)
import Data.Version (Version(..), showVersion)
import System.Cmd (system)
import System.Directory (canonicalizePath, createDirectoryIfMissing,
                         doesDirectoryExist, doesFileExist,
                         getDirectoryContents)
import System.Environment (getProgName)
import System.Exit (ExitCode(..))
import System.IO
import System.Locale (defaultTimeLocale)
import System.Process (runInteractiveCommand, waitForProcess)

import System.FilePath ((</>))
import Distribution.Simple.Compiler (CompilerFlavor(..), Compiler(..),
                                     compilerVersion)
import Distribution.License
import Distribution.Package (PackageIdentifier(..))
import Distribution.Simple.PreProcess (knownSuffixHandlers)
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Configure (configCompiler, configure,
                                      maybeGetPersistBuildConfig)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, distPref)
import Distribution.Simple.SrcDist (createArchive, prepareTree, tarBallName)
import Distribution.Simple.Utils (copyDirectoryRecursiveVerbose,
                                  copyFileVerbose, die, warn)
import Distribution.PackageDescription (BuildInfo(..),
                                        GenericPackageDescription(..),
                                        Library(..),
                                        PackageDescription(..),
                                        emptyHookedBuildInfo,
                                        exeName, flattenPackageDescription,
                                        hasLibs, setupMessage, withExe,
                                        withLib)
import Distribution.Verbosity (Verbosity(..))
import Distribution.Version (Dependency(..), VersionRange(..), withinRange)
import Distribution.Simple.Setup (emptyConfigFlags)
import Distribution.Package.Rpm.Setup (RpmFlags(..))
import System.Posix.Files (setFileCreationMask)

rpm :: GenericPackageDescription -- ^info from the .cabal file
    -> RpmFlags                 -- ^rpm flags
    -> IO ()

rpm genPkgDesc flags = do
    case rpmCompiler flags of
      Just GHC -> return ()
      Just c -> die ("the " ++ show c ++ " compiler is not yet supported")
    if rpmGenSpec flags
      then do
        let pkgDesc = flattenPackageDescription genPkgDesc
        (name, extraDocs) <- createSpecFile False pkgDesc flags "."
        putStrLn $ "Spec file created: " ++ name
        when ((not . null) extraDocs) $ do
            putStrLn "NOTE: docs packaged, but not in .cabal file:"
            mapM_ putStrLn $ sort extraDocs
        return ()
      else rpmBuild genPkgDesc flags

-- | Copy a file or directory (recursively, in the latter case) to the
-- same name in the target directory.  Arguments flipped from the
-- conventional order.

copyTo :: Verbosity -> FilePath -> FilePath -> IO ()

copyTo verbose dest src = do
    isFile <- doesFileExist src
    let destDir = dest </> src
    if isFile
      then copyFileVerbose verbose src destDir
      else copyDirectoryRecursiveVerbose verbose src destDir

autoreconf :: Verbosity -> PackageDescription -> IO ()

autoreconf verbose pkgDesc = do
    ac <- doesFileExist "configure.ac"
    when ac $ do
        c <- doesFileExist "configure"
        when (not c) $ do
            setupMessage verbose "Running autoreconf" pkgDesc
            ret <- system "autoreconf"
            case ret of
              ExitSuccess -> return ()
              ExitFailure n -> die ("autoreconf failed with status " ++ show n)

localBuildInfo :: PackageDescription -> IO LocalBuildInfo
localBuildInfo pkgDesc = do
  mb_lbi <- maybeGetPersistBuildConfig
  case mb_lbi of
    Just lbi -> return lbi
    Nothing -> configure (Right pkgDesc, emptyHookedBuildInfo)
               (emptyConfigFlags defaultProgramConfiguration)

rpmBuild :: GenericPackageDescription -> RpmFlags -> IO ()

rpmBuild genPkgDesc flags = do
    tgtPfx <- canonicalizePath (maybe distPref id $ rpmTopDir flags)
    let pkgDesc = flattenPackageDescription genPkgDesc
        verbose = rpmVerbosity flags
        tmpDir = tgtPfx </> "src"
    flip mapM_ ["BUILD", "RPMS", "SOURCES", "SPECS", "SRPMS"] $ \ subDir -> do
      createDirectoryIfMissing True (tgtPfx </> subDir)
    let specsDir = tgtPfx </> "SPECS"
    lbi <- localBuildInfo pkgDesc
    bracket (setFileCreationMask 0o022) setFileCreationMask $ \ _ -> do
      autoreconf verbose pkgDesc
      (specFile, extraDocs) <- createSpecFile True pkgDesc flags specsDir
      tree <- prepareTree pkgDesc verbose (Just lbi) False tmpDir
              knownSuffixHandlers 0
      mapM_ (copyTo verbose tree) extraDocs
      tarball <- createArchive pkgDesc verbose (Just lbi) tmpDir
                 (tgtPfx </> "SOURCES")
      ret <- system ("rpmbuild -ba --define \"_topdir " ++ tgtPfx ++ "\" " ++
                     specFile)
      case ret of
        ExitSuccess -> return ()
        ExitFailure n -> die ("rpmbuild failed with status " ++ show n)

defaultRelease :: UTCTime -> IO String

defaultRelease now = do
    darcsRepo <- doesDirectoryExist "_darcs"
    return $ if darcsRepo
               then formatTime defaultTimeLocale "0.%Y%m%d" now
               else "1"

rstrip :: (Char -> Bool) -> String -> String

rstrip p = reverse . dropWhile p . reverse

createSpecFile :: Bool                -- ^whether to forcibly create file
               -> PackageDescription  -- ^info from the .cabal file
               -> RpmFlags            -- ^rpm flags
               -> FilePath            -- ^directory in which to create file
               -> IO (FilePath, [FilePath])

createSpecFile force pkgDesc flags tgtPfx = do
    (compiler, _) <- configCompiler (rpmCompiler flags) Nothing Nothing
                     defaultProgramConfiguration
                     (rpmVerbosity flags)
    now <- getCurrentTime
    defRelease <- defaultRelease now
    let pkg = package pkgDesc
        verbose = rpmVerbosity flags
        origName = pkgName pkg
        name = maybe (map toLower origName) id (rpmName flags)
        version = maybe ((showVersion . pkgVersion) pkg) id (rpmVersion flags)
        release = maybe defRelease id (rpmRelease flags)
        specPath = tgtPfx </> name ++ ".spec"
        group = "Development/Languages"
        buildRoot = "%{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)"
        cmplrVersion = compilerVersion compiler
        doHaddock = rpmHaddock flags && hasLibs pkgDesc
        (cmplr, runner) = case compilerFlavor compiler of
             GHC -> ("ghc", "runghc")
             Hugs -> ("hugs", "runhugs")
             JHC -> ("jhc", "runjhc")
             NHC -> ("nhc", "runnhc")
             (OtherCompiler s) -> (s, "run" ++ s)
    unless force $ do
        specAlreadyExists <- doesFileExist specPath
        when specAlreadyExists $
            die $ "spec file already exists: " ++ specPath
    h <- openFile specPath WriteMode
    buildReq <- showBuildReq verbose doHaddock compiler pkgDesc
    runtimeReq <- showRuntimeReq verbose compiler pkgDesc
    progName <- getProgName
    let putHdr hdr val = hPutStrLn h (hdr ++ ": " ++ val)
        putHdr_ hdr val = when (not $ null val) $
                              hPutStrLn h (hdr ++ ": " ++ val)
        putHdrD hdr val dfl = hPutStrLn h (hdr ++ ": " ++
                                           if null val then dfl else val)
        putNewline = hPutStrLn h ""
        put s = hPutStrLn h s
        putDef v s = put $ "%define " ++ v ++ ' ' : s
        putSetup s = put $ runner ++ " Setup " ++ s
        date = formatTime defaultTimeLocale "%a %b %d %Y" now
    putDef "hsc_name" cmplr
    putDef "hsc_version" $ showVersion cmplrVersion
    putDef "hsc_namever" $ compilerNameVersion compiler
    putDef "pkg_name" origName
    putDef "pkg_libdir" "%{_libdir}/%{hsc_name}-%{hsc_version}/%{pkg_name}-%{version}"
    putNewline

    putHdr "Name" name
    putHdr "Version" version
    putHdr "Release" $ release ++ "%{?dist}"
    putHdr "License" $ (showLicense . license) pkgDesc
    putHdr "Group" group
    putHdr_ "URL" $ homepage pkgDesc
    putHdr "Source" $ tarBallName pkgDesc
    -- Some packages conflate the synopsis and description fields.  Ugh.
    let syn = synopsis pkgDesc
        syn' = (head . lines) syn
        synTooLong = syn /= syn'
        summary = if synTooLong
                  then syn' ++ " [...]"
                  else rstrip (== '.') syn'
    when synTooLong $
        warn verbose "The synopsis for this package spans multiple lines."
    putHdrD "Summary" summary "This package has no summary"
    putHdr "BuildRoot" buildRoot
    putHdr "BuildRequires" buildReq
    -- External libraries incur both build-time and runtime
    -- dependencies.  The latter only need to be made explicit for the
    -- built library, as RPM is smart enough to ferret out good
    -- dependencies for binaries.
    extDeps <- withLib pkgDesc [] (findLibDeps .libBuildInfo)
    let extraReq = concat $ intersperse ", " extDeps
    putHdr_ "BuildRequires" extraReq
    putNewline
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

    {- Compiler-specific library data goes into a package of its own.<

       Unlike a library for a traditional language, the library
       packagen depends on the compiler, because when installed, it
       has to register itself with the compiler's own package
       management system. -}

    withLib pkgDesc () $ \_ -> do
        put "%package -n %{hsc_namever}-%{name}"
        putHdrD "Summary" summary "This library package has no summary"
        putHdr "Group" "Development/Libraries"
        putHdr "Requires" "%{hsc_name} = %{hsc_version}"
        putHdr_ "Requires" extraReq
        putHdr_ "Requires" runtimeReq
        putHdr "Provides" "%{pkg_name}-%{hsc_namever} = %{version}"
        putNewline
        putNewline

        put "%description -n %{hsc_namever}-%{name}"
        putDesc
        putNewline
        put "This package contains libraries for %{hsc_name} %{hsc_version}."
        putNewline
        putNewline

    when (rpmLibProf flags) $ do
        put "%package -n %{hsc_namever}-%{name}-prof"
        putHdr "Summary" "Profiling libraries for %{hsc_namever}-%{name}"
        putHdr "Group" "Development/Libraries"
        putHdr "Requires" "%{hsc_namever}-%{name} = %{version}"
        putHdr "Provides" "%{pkg_name}-%{hsc_namever}-prof = %{version}"
        putNewline
        putNewline

        put "%description -n %{hsc_namever}-%{name}-prof"
        putDesc
        putNewline
        put "This package contains profiling libraries for %{hsc_name} %{hsc_version}."
        putNewline
        putNewline

    put "%prep"
    put $ "%setup -q -n %{pkg_name}-%{version}"
    putNewline
    putNewline

    put "%build"
    put "if [ -f configure.ac -a ! -f configure ]; then autoreconf; fi"
    putSetup ("configure --prefix=%{_prefix} --libdir=%{_libdir} " ++
              "--docdir=%{_docdir}/%{hsc_namever}-%{name}-%{version} " ++
              "--libsubdir='$compiler/$pkgid' " ++
              (if (rpmLibProf flags) then "--enable" else "--disable") ++
              "-library-profiling --" ++ cmplr)
    withLib pkgDesc () $ \_ -> do
        hPutStr h "if "
        putSetup "makefile -f cabal-rpm.mk"
        put "then"
        put "    make -f cabal-rpm.mk %{_smp_mflags} || :"
        put "fi"
    putSetup "build"
    withLib pkgDesc () $ \_ -> do
        when doHaddock $
            putSetup "haddock || :"
        putSetup "register --gen-script"
        putSetup "unregister --gen-script"
    putNewline
    putNewline

    docs <- findDocs pkgDesc

    put "%install"
    put "rm -rf ${RPM_BUILD_ROOT}"
    putSetup "copy --destdir=${RPM_BUILD_ROOT}"
    withLib pkgDesc () $ \_ -> do
        put "install -m 755 register.sh unregister.sh ${RPM_BUILD_ROOT}%{pkg_libdir}"
        put "cd ${RPM_BUILD_ROOT}"
        put "echo '%defattr(-,root,root,-)' > %{_builddir}/%{?buildsubdir}/%{name}-files.prof"
        put "find .%{pkg_libdir} \\( -name '*_p.a' -o -name '*.p_hi' \\) | sed s/^.// >> %{_builddir}/%{?buildsubdir}/%{name}-files.prof"
        put "echo '%defattr(-,root,root,-)' > %{_builddir}/%{?buildsubdir}/%{name}-files.nonprof"
        put "find .%{pkg_libdir} -type d | sed 's/^./%dir /' >> %{_builddir}/%{?buildsubdir}/%{name}-files.nonprof"
        put "find .%{pkg_libdir} ! \\( -type d -o -name '*_p.a' -o -name '*.p_hi' \\) | sed s/^.// >> %{_builddir}/%{?buildsubdir}/%{name}-files.nonprof"
        put "sed 's,^/,%exclude /,' %{_builddir}/%{?buildsubdir}/%{name}-files.prof >> %{_builddir}/%{?buildsubdir}/%{name}-files.nonprof"
    putNewline
    put "cd ${RPM_BUILD_ROOT}/%{_datadir}/%{pkg_name}-%{version}"
    put $ "rm -rf doc " ++ concat (intersperse " " docs)
    putNewline
    putNewline

    put "%clean"
    put "rm -rf ${RPM_BUILD_ROOT}"
    putNewline
    putNewline

    withLib pkgDesc () $ \_ -> do
        {- If we're upgrading to a library with the same Cabal
           name+version as the currently installed library (i.e. we've
           just bumped the release number), we need to unregister the
           old library first, so that the register script in %post may
           succeed.

           Note that this command runs *before* the new package's
           files are installed, and thus will execute the *previous*
           version's unregister script, if the script exists in the
           same location as the about-to-be-installed library's
           script. -}

        put "%pre -n %{hsc_namever}-%{name}"
        put "[ \"$1\" = 2 ] && %{pkg_libdir}/unregister.sh >&/dev/null || :"
        putNewline
        putNewline

        put "%post -n %{hsc_namever}-%{name}"
        put "%{pkg_libdir}/register.sh >&/dev/null"
        putNewline
        putNewline

        {- We must unregister an old version during an upgrade as
           well as during a normal removal, otherwise the Haskell
           runtime's package system will be left with a phantom record
           for a package it can no longer use. -}

        put "%preun -n %{hsc_namever}-%{name}"
        put "%{pkg_libdir}/unregister.sh >&/dev/null"
        putNewline
        putNewline

        {- If we're upgrading, the %preun step may have unregistered
           the *new* version of the library (if it had an identical
           Cabal name+version, even though the RPM release differs);
           therefore, we must attempt to re-register it. -}

        put "%postun -n %{hsc_namever}-%{name}"
        put "[ \"$1\" = 1 ] && %{pkg_libdir}/register.sh >& /dev/null || :"
        putNewline
        putNewline

        put "%files -n %{hsc_namever}-%{name} -f %{name}-files.nonprof"
        when doHaddock $
            put "%doc dist/doc/html"
        when ((not . null) docs) $
            put $ "%doc " ++ concat (intersperse " " docs)
        putNewline
        putNewline

        when (rpmLibProf flags) $ do
            put "%files -n %{hsc_namever}-%{name}-prof -f %{name}-files.prof"
            putNewline
            putNewline

    put "%files"
    put "%defattr(-,root,root,-)"
    withExe pkgDesc $ \exe -> put $ "%{_bindir}/" ++ exeName exe
    when ((not . null . dataFiles) pkgDesc) $
        put "%{_datadir}/%{pkg_name}-%{version}"

    -- Add the license file to the main package only if it wouldn't
    -- otherwise be empty.
    when ((not . null . licenseFile) pkgDesc &&
          ((not . null . executables) pkgDesc ||
           (not . null . dataFiles) pkgDesc)) $
        put $ "%doc " ++ licenseFile pkgDesc
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
             else let license = licenseFile pkgDesc
                  in license : filter (/= license) docs
  where names = ["author", "copying", "doc", "example", "licence", "license",
                 "readme", "todo"]
        likely name = let lowerName = map toLower name
                      in any (`isPrefixOf` lowerName) names

-- | Take a Haskell package name, and turn it into a "virtual package"
-- that encodes the compiler name and version used.

virtualPackage :: Compiler -> String -> String
virtualPackage compiler name = name ++ '-' : compilerNameVersion compiler

compilerNameVersion :: Compiler -> String
compilerNameVersion (Compiler flavour (PackageIdentifier _ version) _) =
    name ++ squishedVersion
  where name = case flavour of
               GHC -> "ghc"
               Hugs -> "hugs"
               JHC -> "jhc"
               NHC -> "nhc"
        squishedVersion = (concat . map show . versionBranch) version

-- | Convert from license to RPM-friendly description.  The strings are
-- taken from TagsCheck.py in the rpmlint distribution.

showLicense :: License -> String
showLicense GPL = "GPL"
showLicense LGPL = "LGPL"
showLicense BSD3 = "BSD"
showLicense BSD4 = "BSD-like"
showLicense PublicDomain = "Public Domain"
showLicense AllRightsReserved = "Proprietary"
showLicense OtherLicense = "Non-distributable"

-- | Determine whether a specific version of a Haskell package is "built
-- into" this particular version of the given compiler.

isBuiltIn :: CompilerFlavor
        -> Version
        -> Dependency
        -> Bool
isBuiltIn cn cv (Dependency pkg version) = maybe False checkVersion $ do
    (_, _, cb) <- find (\(n, v, _) -> (n,v) == (cn,cv)) builtIns
    (_, pv) <- find (\(pn, _) -> pn == pkg) cb
    return pv
  where checkVersion = flip withinRange version
        builtIns = [(GHC, Version [6,6,1] [], ghc661BuiltIns),
                    (GHC, Version [6,6] [], ghc66BuiltIns)]
        v n x = (n, Version x [])
        ghc661BuiltIns = [
            v "base" [2,1,1],
            v "Cabal" [1,1,6,2],
            v "cgi" [3001,1,1],
            v "fgl" [5,4,1],
            v "filepath" [1,0],
            v "ghc" [6,6,1],
            v "GLUT" [2,1,1],
            v "haskell98" [1,0],
            v "haskell-src" [1,0,1],
            v "HGL" [3,1,1],
            v "html" [1,0,1],
            v "HUnit" [1,1,1],
            v "mtl" [1,0,1],
            v "network" [2,0,1],
            v "OpenAL" [1,3,1],
            v "OpenGL" [2,2,1],
            v "parsec" [2,0],
            v "QuickCheck" [1,0,1],
            v "readline" [1,0],
            v "regex-base" [0,72],
            v "regex-compat" [0,71],
            v "regex-posix" [0,71],
            v "rts" [1,0],
            v "stm" [2,0],
            v "template-haskell" [2,1],
            v "time" [1,1,1],
            v "unix" [2,1],
            v "X11" [1,2,1],
            v "xhtml" [3000,0,2]
                         ]
        ghc66BuiltIns = [
            v "base" [2,0],
            v "Cabal" [1,1,6],
            v "cgi" [2006,9,6],
            v "fgl" [5,2],
            v "ghc" [6,6],
            v "GLUT" [2,0],
            v "haskell98" [1,0],
            v "haskell-src" [1,0],
            v "HGL" [3,1],
            v "html" [1,0],
            v "HTTP" [2006,7,7],
            v "HUnit" [1,1],
            v "mtl" [1,0],
            v "network" [2,0],
            v "OpenAL" [1,3],
            v "OpenGL" [2,1],
            v "parsec" [2,0],
            v "QuickCheck" [1,0],
            v "readline" [1,0],
            v "regex-base" [0,71],
            v "regex-compat" [0,71],
            v "regex-posix" [0,71],
            v "rts" [1,0],
            v "stm" [2,0],
            v "template-haskell" [2,0],
            v "time" [1,0],
            v "unix" [1,0],
            v "X11" [1,1],
            v "xhtml" [2006,9,13]
                        ]

-- | Generate a string expressing runtime dependencies, but only
-- on package/version pairs not already "built into" a compiler
-- distribution.

showRuntimeReq :: Verbosity -> Compiler -> PackageDescription -> IO String

showRuntimeReq verbose c@(Compiler cFlav (PackageIdentifier _ cVersion) _)
               pkgDesc = do
    let externalDeps = filter (not . isBuiltIn cFlav cVersion)
                       (buildDepends pkgDesc)
    clauses <- mapM (showRpmReq verbose (virtualPackage c)) externalDeps
    return $ (concat . intersperse ", " . concat) clauses

-- | Generate a string expressing package build dependencies, but only
-- on package/version pairs not already "built into" a compiler
-- distribution.

showBuildReq :: Verbosity -> Bool -> Compiler -> PackageDescription
             -> IO String

showBuildReq verbose haddock
             c@(Compiler cFlav (PackageIdentifier _ cVersion) _) pkgDesc =
  do
    cPkg <- case cFlav of
              GHC -> return "ghc"
              Hugs -> return "hugs98"
              _ -> die $ "unknown compiler " ++ show cFlav
    let myDeps = [Dependency cPkg (ThisVersion cVersion)] ++
                 if haddock then [Dependency "haddock" AnyVersion] else []
        externalDeps = filter (not . isBuiltIn cFlav cVersion)
                       (buildDepends pkgDesc)
    exReqs <- mapM (showRpmReq verbose (virtualPackage c)) externalDeps
    myReqs <- mapM (showRpmReq verbose id) myDeps
    return $ (concat . intersperse ", " . concat) (myReqs ++ exReqs)

-- | Represent a dependency in a form suitable for an RPM spec file.

showRpmReq :: Verbosity -> (String -> String) -> Dependency -> IO [String]

showRpmReq verbose f (Dependency pkg AnyVersion) =
    return [f pkg]
showRpmReq verbose f (Dependency pkg (ThisVersion v)) =
    return [f pkg ++ " = " ++ showVersion v]
showRpmReq verbose f (Dependency pkg (EarlierVersion v)) =
    return [f pkg ++ " < " ++ showVersion v]
showRpmReq verbose f (Dependency pkg (LaterVersion v)) =
    return [f pkg ++ " > " ++ showVersion v]
showRpmReq verbose f (Dependency pkg (UnionVersionRanges
                         (ThisVersion v1)
                         (LaterVersion v2)))
    | v1 == v2 = return [f pkg ++ " >= " ++ showVersion v1]
showRpmReq verbose f (Dependency pkg (UnionVersionRanges
                         (ThisVersion v1)
                         (EarlierVersion v2)))
    | v1 == v2 = return [f pkg ++ " <= " ++ showVersion v1]
showRpmReq verbose f (Dependency pkg (UnionVersionRanges _ _)) = do
    warn verbose ("Cannot accurately represent " ++
                  "dependency on package " ++ f pkg)
    warn verbose "  (uses version union, which RPM can't handle)"
    return [f pkg]
showRpmReq verbose f (Dependency pkg (IntersectVersionRanges r1 r2)) = do
    d1 <- showRpmReq verbose f (Dependency pkg r1)
    d2 <- showRpmReq verbose f (Dependency pkg r2)
    return (d1 ++ d2)

-- | Find the paths to all "extra" libraries specified in the package
-- config.  Prefer shared libraries, since that's what gcc prefers.
findLibPaths :: BuildInfo -> IO [FilePath]

findLibPaths buildInfo = mapM findLib (extraLibs buildInfo)
  where findLib :: String -> IO FilePath
        findLib lib = do
            so <- findLibPath ("lib" ++ lib ++ ".so")
            if isJust so
              then return (fromJust so)
              else findLibPath ("lib" ++ lib ++ ".a") >>=
                   maybe (die $ "could not find library: lib" ++ lib)
                         return
        findLibPath extraLib = do
            loc <- findInExtraLibs (extraLibDirs buildInfo)
            if isJust loc
              then return loc
              else findWithGcc extraLib
          where findInExtraLibs (d:ds) = do
                    let path = d </> extraLib
                    exists <- doesFileExist path
                    if exists
                      then return (Just path)
                      else findInExtraLibs ds
                findInExtraLibs [] = return Nothing

-- | Return the full path to a file (usually an object file) that gcc
-- knows about.

findWithGcc :: FilePath -> IO (Maybe FilePath)

findWithGcc lib = do
    (i,o,e,p) <- runInteractiveCommand $ "gcc -print-file-name=" ++ lib
    loc <- hGetLine o
    mapM_ hClose [i,o,e]
    waitForProcess p
    return $ if loc == lib then Nothing else Just loc

-- | Return the RPM that owns a particular file or directory.  Die if
-- not owned.

findRpmOwner :: FilePath -> IO String
findRpmOwner path = do
    (i,o,e,p) <- runInteractiveCommand (rpmQuery ++ path)
    pkg <- hGetLine o
    mapM_ hClose [i,o,e]
    ret <- waitForProcess p
    case ret of
      ExitSuccess -> return pkg
      _ -> die $ "not owned by any package: " ++ path
  where rpmQuery = "rpm --queryformat='%{NAME}' -qf "

-- | Find all RPMs on which the build of this package depends.  Die if
-- a dependency is not present, or not owned by an RPM.

findLibDeps :: BuildInfo -> IO [String]

findLibDeps buildInfo = findLibPaths buildInfo >>= mapM findRpmOwner
