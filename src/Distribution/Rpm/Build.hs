-- |
-- Module      :  Distribution.Package.Rpm
-- Copyright   :  Bryan O'Sullivan 2007, 2008
--                Jens Petersen 2012-2013
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Support for building RPM packages.  Can also generate
-- an RPM spec file if you need a basic one to hand-customize.

-- This software may be used and distributed according to the terms of
-- the GNU General Public License, incorporated herein by reference.

module Distribution.Rpm.Build (
    rpmBuild
    ) where

--import Control.Exception (bracket)
import Control.Monad    (filterM, liftM, unless, when)

import Distribution.PackageDescription (GenericPackageDescription (..),
                                        PackageDescription (..),
                                        hasExes)

--import Distribution.Version (VersionRange, foldVersionRange')

import Distribution.Rpm.PackageUtils (packageName, packageVersion,
                                      simplePackageDescription)
import Distribution.Rpm.Setup (RpmFlags (..))
import Distribution.Rpm.Spec (createSpecFile)
import Distribution.Rpm.SysCmd (tryReadProcess, trySystem, optionalSudo, systemBool, (+-+))

import System.Directory (copyFile, doesFileExist,
                         getCurrentDirectory)
import System.Environment (getEnv)
import System.FilePath.Posix ((</>))

-- autoreconf :: Verbosity -> PackageDescription -> IO ()
-- autoreconf verbose pkgDesc = do
--     ac <- doesFileExist "configure.ac"
--     when ac $ do
--         c <- doesFileExist "configure"
--         when (not c) $ do
--             setupMessage verbose "Running autoreconf" pkgDesc
--             trySystem "autoreconf"

rpmBuild :: FilePath -> GenericPackageDescription -> RpmFlags -> Bool -> IO ()
rpmBuild cabalPath genPkgDesc flags binary = do
--    let verbose = rpmVerbosity flags
    pkgDesc <- simplePackageDescription genPkgDesc flags
--    bracket (setFileCreationMask 0o022) setFileCreationMask $ \ _ -> do
--      autoreconf verbose pkgDesc
    specFile <- specFileName pkgDesc flags
    specFileExists <- doesFileExist specFile
    if specFileExists
      then putStrLn $ "Using existing" +-+ specFile
      else createSpecFile cabalPath genPkgDesc flags
    let pkg = package pkgDesc
        name = packageName pkg
    when binary $ do
      br_out <- tryReadProcess "rpmspec" ["-q", "--buildrequires", specFile]
      missing <- filterM notInstalled $ lines br_out
      unless (null missing) $ do
        putStrLn $ "Installing dependencies:"
        mapM_ putStrLn missing
        optionalSudo ("yum install" +-+ unwords missing)

    cwd <- getCurrentDirectory
    home <- getEnv "HOME"
    let version = packageVersion pkg
        cachedir = home </> ".cabal/packages/hackage.haskell.org" </> name </> version
        tarFile = name ++ "-" ++ version ++ ".tar.gz"
        rpmCmd = if binary then "a" else "s"
    tarFileExists <- doesFileExist tarFile
    unless tarFileExists $ do
      trySystem ("cabal fetch -v0 --no-dependencies" +-+ name ++ "-" ++ version)
      copyFile (cachedir </> tarFile) (cwd </> tarFile)
    trySystem ("rpmbuild -b" ++ rpmCmd +-+
                     "--define \"_rpmdir" +-+ cwd ++ "\"" +-+
                     "--define \"_srcrpmdir" +-+ cwd ++ "\"" +-+
                     "--define \"_sourcedir" +-+ cwd ++ "\"" +-+
                     specFile)
  where
    notInstalled :: String -> IO Bool
    notInstalled br = do
      liftM not $ systemBool $ "rpm -q" +-+ br

specFileName :: PackageDescription    -- ^pkg description
               -> RpmFlags            -- ^rpm flags
               -> IO FilePath
specFileName pkgDesc flags = do
    let pkg = package pkgDesc
        name = packageName pkg
        pkgname = if isExec then name else "ghc-" ++ name
        isExec = if (rpmLibrary flags) then False else hasExes pkgDesc
    return $ pkgname ++ ".spec"
