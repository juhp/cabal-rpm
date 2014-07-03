-- |
-- Module      :  Commands.RpmBuild
-- Copyright   :  (C) 2007-2008  Bryan O'Sullivan
--                (C) 2012-2014  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Support for building RPM packages.  Can also generate
-- an RPM spec file if you need a basic one to hand-customize.

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Commands.RpmBuild (
    rpmBuild, rpmBuild_, RpmStage (..)
    ) where

import Commands.Spec (createSpecFile)
import FileUtils (fileWithExtension, getDirectoryContents_)
import PackageUtils (isScmDir, missingPackages, packageName, packageVersion)
import Setup (RpmFlags (..))
import SysCmd (cmd_, yumInstall, (+-+))

--import Control.Exception (bracket)
import Control.Monad    (filterM, unless, void, when)

import Distribution.PackageDescription (PackageDescription (..),
                                        hasExes)

--import Distribution.Version (VersionRange, foldVersionRange')

import System.Directory (copyFile, doesFileExist, getCurrentDirectory)
import System.Environment (getEnv)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (setFileMode, getFileStatus, fileMode)

-- autoreconf :: Verbosity -> PackageDescription -> IO ()
-- autoreconf verbose pkgDesc = do
--     ac <- doesFileExist "configure.ac"
--     when ac $ do
--         c <- doesFileExist "configure"
--         when (not c) $ do
--             setupMessage verbose "Running autoreconf" pkgDesc
--             cmd_ "autoreconf" []

data RpmStage = Binary | Source | Prep | BuildDep deriving Eq

rpmBuild :: FilePath -> PackageDescription -> RpmFlags -> RpmStage -> IO FilePath
rpmBuild cabalPath pkgDesc flags stage = do
--    let verbose = rpmVerbosity flags
--    bracket (setFileCreationMask 0o022) setFileCreationMask $ \ _ -> do
--      autoreconf verbose pkgDesc
    mspcfile <- fileWithExtension "." ".spec"
    specFile <- case mspcfile of
      Just s -> return s
      Nothing -> specFileName pkgDesc flags
    specFileExists <- doesFileExist specFile
    if specFileExists
      then putStrLn $ "Using existing" +-+ specFile
      else createSpecFile cabalPath pkgDesc flags Nothing
    let pkg = package pkgDesc
        name = packageName pkg
    when (stage `elem` [Binary,BuildDep]) $ do
      missing <- missingPackages pkgDesc name
      yumInstall missing True

    unless (stage == BuildDep) $ do
      let version = packageVersion pkg
          tarFile = name ++ "-" ++ version ++ ".tar.gz"
          rpmCmd = case stage of
            Binary -> "a"
            Source -> "s"
            Prep -> "p"
            BuildDep -> "_"

      tarFileExists <- doesFileExist tarFile
      unless tarFileExists $ do
        scmRepo <- isScmDir $ takeDirectory cabalPath
        when scmRepo $
          error "No tarball for source repo"

      cwd <- getCurrentDirectory
      copyTarball name version False
      cmd_ "rpmbuild" $ ["-b" ++ rpmCmd] ++
                 ["--nodeps" | stage == Prep] ++
                 ["--define=_rpmdir" +-+ cwd,
                 "--define=_srcrpmdir" +-+ cwd,
                 "--define=_sourcedir" +-+ cwd,
                 specFile]
    return specFile
  where
    copyTarball :: String -> String -> Bool -> IO ()
    copyTarball n v ranFetch = do
      let tarfile = n ++ "-" ++ v ++ ".tar.gz"
      already <- doesFileExist tarfile
      unless already $ do
        home <- getEnv "HOME"
        let cacheparent = home </> ".cabal" </> "packages"
            tarpath = n </> v </> tarfile
        remotes <- getDirectoryContents_ cacheparent
        let paths = map (\ repo -> cacheparent </> repo </> tarpath) remotes
        -- if more than one tarball, should maybe warn if they are different
        tarballs <- filterM doesFileExist paths
        if null tarballs
          then if ranFetch
               then error $ "No" +-+ tarfile +-+ "found"
               else do
                 cmd_ "cabal" ["fetch", "-v0", "--no-dependencies", n ++ "-" ++ v]
                 copyTarball n v True
          else do
            copyFile (head tarballs) tarfile
            -- cabal fetch creates tarballs with mode 0600
            stat <- getFileStatus tarfile
            when (fileMode stat /= 0o100644) $
              setFileMode tarfile 0o0644

rpmBuild_ :: FilePath -> PackageDescription -> RpmFlags -> RpmStage -> IO ()
rpmBuild_ cabalPath pkgDesc flags stage =
  void (rpmBuild cabalPath pkgDesc flags stage)

specFileName :: PackageDescription    -- ^pkg description
               -> RpmFlags            -- ^rpm flags
               -> IO FilePath
specFileName pkgDesc flags = do
    let pkg = package pkgDesc
        name = packageName pkg
        pkgname = if isExec then name else "ghc-" ++ name
        isExec = not (rpmLibrary flags) && hasExes pkgDesc
    return $ pkgname ++ ".spec"
