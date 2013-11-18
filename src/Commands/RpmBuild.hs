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
import PackageUtils (packageName, packageVersion,
                                      simplePackageDescription)
import Setup (RpmFlags (..))
import SysCmd (tryReadProcess, trySystem, optionalSudo, systemBool, (+-+))

--import Control.Exception (bracket)
import Control.Applicative ((<$>))
import Control.Monad    (filterM, liftM, unless, when)

import Data.List (isPrefixOf)
import Data.Maybe (isNothing)

import Distribution.PackageDescription (GenericPackageDescription (..),
                                        PackageDescription (..),
                                        hasExes)

--import Distribution.Version (VersionRange, foldVersionRange')

import System.Directory (copyFile, doesFileExist, doesDirectoryExist,
                         getCurrentDirectory, getDirectoryContents)
import System.Environment (getEnv)
import System.FilePath.Posix (takeDirectory, (</>))

-- We could use the Safe package instead, but since this is very simple...
headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

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
        putStrLn "Installing dependencies:"
        mapM_ putStrLn missing
        optionalSudo $ "yum install" +-+ unwords (map show missing)

    cwd <- getCurrentDirectory
    home <- getEnv "HOME"
    let version = packageVersion pkg
        cacheparent = home </> ".cabal" </> "packages"
        tarFile = name ++ "-" ++ version ++ ".tar.gz"
        relativeToCache = name </> version </> tarFile
        rpmCmd = if binary then "a" else "s"

    exists <- existsInCache cacheparent relativeToCache
    -- If it exists, it'll be at (Just s) -> s </> relativeToCache
    unless (isNothing exists) $ do
      let pkgDir = takeDirectory cabalPath
      darcsRepo <- doesDirectoryExist $ pkgDir </> "_darcs"
      unless darcsRepo $ do
        gitRepo <- doesDirectoryExist $ pkgDir </> ".git"
        unless (darcsRepo || gitRepo) $ do
          trySystem ("cabal fetch -v0 --no-dependencies" +-+ name ++ "-" ++ version)
          exists' <- existsInCache cacheparent relativeToCache
          case exists' of
            Nothing -> error "Unable to fetch sources."
            Just s  -> copyFile (s </> relativeToCache) (cwd </> tarFile)
      exists' <- existsInCache cacheparent relativeToCache
      case exists' of
        Nothing -> error $ "No" +-+ tarFile +-+ "found"
        Just _  -> trySystem ("rpmbuild -b" ++ rpmCmd +-+
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
    existsInCache :: FilePath -> FilePath -> IO (Maybe FilePath)
    existsInCache cacheparent relativeToCache = do
      cachepath <- fmap (cacheparent </>) <$> filter (not . isPrefixOf ".") <$> getDirectoryContents cacheparent
      x <- filterM (\x -> doesFileExist (x </> relativeToCache)) cachepath
      return $ headMay x

specFileName :: PackageDescription    -- ^pkg description
               -> RpmFlags            -- ^rpm flags
               -> IO FilePath
specFileName pkgDesc flags = do
    let pkg = package pkgDesc
        name = packageName pkg
        pkgname = if isExec then name else "ghc-" ++ name
        isExec = not (rpmLibrary flags) && hasExes pkgDesc
    return $ pkgname ++ ".spec"
