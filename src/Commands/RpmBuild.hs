-- |
-- Module      :  Commands.RpmBuild
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

module Commands.RpmBuild (
    rpmBuild
    ) where

import Commands.Spec (createSpecFile)
import PackageUtils (isScmDir, packageName, packageVersion,
                                      simplePackageDescription)
import Setup (RpmFlags (..))
import SysCmd (tryReadProcess, trySystem, systemBool, yumInstall, (+-+))

--import Control.Exception (bracket)
import Control.Applicative ((<$>))
import Control.Monad    (filterM, liftM, unless, when)

import Data.List (isPrefixOf)

import Distribution.PackageDescription (GenericPackageDescription (..),
                                        PackageDescription (..),
                                        hasExes)

--import Distribution.Version (VersionRange, foldVersionRange')

import System.Directory (copyFile, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)
import System.Environment (getEnv)
import System.FilePath.Posix (takeDirectory, (</>))

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
      yumInstall missing

    let version = packageVersion pkg
        tarFile = name ++ "-" ++ version ++ ".tar.gz"
        rpmCmd = if binary then "a" else "s"

    tarFileExists <- doesFileExist tarFile
    unless tarFileExists $ do
      scmRepo <- isScmDir $ takeDirectory cabalPath
      when scmRepo $
        error "No tarball for source repo"

    cwd <- getCurrentDirectory
    copyTarball name version False cwd
    trySystem ("rpmbuild -b" ++ rpmCmd +-+
               "--define \"_rpmdir" +-+ cwd ++ "\"" +-+
               "--define \"_srcrpmdir" +-+ cwd ++ "\"" +-+
               "--define \"_sourcedir" +-+ cwd ++ "\"" +-+
               specFile)
  where
    notInstalled :: String -> IO Bool
    notInstalled br =
      liftM not $ systemBool $ "rpm -q --whatprovides" +-+ shellQuote br
    shellQuote :: String -> String
    shellQuote (c:cs) = (if c `elem` "()" then (['\\', c] ++) else (c:)) (shellQuote cs)
    shellQuote "" = ""
    copyTarball :: String -> String -> Bool -> FilePath -> IO ()
    copyTarball n v ranFetch dest = do
      home <- getEnv "HOME"
      let cacheparent = home </> ".cabal" </> "packages"
          tarfile = n ++ "-" ++ v ++ ".tar.gz"
          tarpath = n </> v </> tarfile
      remotes <- filter (not . isPrefixOf ".") <$> getDirectoryContents cacheparent
      let paths = map (\ repo -> cacheparent </> repo </> tarpath) remotes
      -- if more than one tarball, should maybe warn if they are different
      tarballs <- filterM doesFileExist paths
      if null tarballs
        then if ranFetch
             then error $ "No" +-+ tarfile +-+ "found"
             else do
               trySystem ("cabal fetch -v0 --no-dependencies" +-+ n ++ "-" ++ v)
               copyTarball n v True dest
        else copyFile (head tarballs) (dest </> tarfile)

specFileName :: PackageDescription    -- ^pkg description
               -> RpmFlags            -- ^rpm flags
               -> IO FilePath
specFileName pkgDesc flags = do
    let pkg = package pkgDesc
        name = packageName pkg
        pkgname = if isExec then name else "ghc-" ++ name
        isExec = not (rpmLibrary flags) && hasExes pkgDesc
    return $ pkgname ++ ".spec"
