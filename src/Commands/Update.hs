{-# LANGUAGE CPP #-}

-- |
-- Module      :  Commands.Update
-- Copyright   :  (C) 2014-2020  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
--
-- Explanation: update spec file to a new package version

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Commands.Update (
  update
  ) where

import Commands.Spec (createSpecFile)
import FileUtils (withTempDirectory)
import Header (headerOption, withSpecHead)
import PackageUtils (PackageData (..), bringTarball, editSpecField,
                     getRevisedCabal, getSpecField, latestPackage,
                     patchSpec, pkgSpecPkgData, prepare)
import Stackage (defaultLTS)
import SysCmd (die)
import Types

import SimpleCabal (PackageDescription (customFieldsPD, package),
                    PackageIdentifier (..), showVersion)
import SimpleCmd (cmd_, error', grep_, shell_, (+-+))
import SimpleCmd.Git (grepGitConfig, rwGitDir)
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (unless, when)
import Data.Maybe (isJust)
import Distribution.Text (display)
import Distribution.Verbosity (silent)
import System.Directory (createDirectory, renameFile, setCurrentDirectory)
import System.FilePath ((</>), (<.>))

update :: Maybe PackageVersionSpecifier -> IO ()
update mpvs = do
  pkgdata <- pkgSpecPkgData [] (pvsPackage =<< mpvs) True
  case specFilename pkgdata of
    Nothing -> die "No (unique) .spec file in directory."
    Just spec -> do
      let pkgDesc = packageDesc pkgdata
          oldPkgId = package pkgDesc
          name = pkgName oldPkgId
          revised = isJust $ lookup "x-revision" (customFieldsPD pkgDesc)
      newPkgId <- case mpvs of
                    Nothing -> do
                      stream <-
                        withSpecHead spec $ \ headerwords -> do
                        let mspecstream = read <$> headerOption "--stream" headerwords
                        case mspecstream of
                          Just specStream -> do
                            putStrLn $ "Using stream" +-+ showStream specStream  +-+ "from spec file"
                            when (specStream <= defaultLTS) $
                              putStrLn $ "Warning: < current default stream" +-+ showStream defaultLTS
                          Nothing -> return ()
                        return mspecstream
                      latestPackage stream name
                    Just pvs ->
                      case pvs of
                        PVStreamPackage stream mpkg ->
                          case mpkg of
                            Just pkg | pkg /= name -> error' "different package name"
                            _ -> latestPackage (Just stream) name
                        PVPackageId pkgid -> return pkgid
                        PVPackageName pkg -> latestPackage Nothing pkg

      let newver = pkgVersion newPkgId
          oldver = pkgVersion oldPkgId
          updated = newver > oldver
      if newver < oldver
        then putStrLn $ "current" +-+ display oldver +-+ "is newer!"
        else do
        getRevisedCabal newPkgId
        unless updated $
          putStrLn "Package is already latest version."
        when (not revised || updated) $
          withTempDirectory $ \cwd -> do
          let specfile = cwd </> spec
          subpkg <- grep_ "%{subpkgs}" specfile
          (curspec, _) <- createSpecVersion oldPkgId specfile revised subpkg
          (newspec, newrevised) <- createSpecVersion newPkgId specfile True subpkg
          currel <- getSpecField "Release" specfile
          let suffix = "%{?dist}"
              defrelease = "1"
          editSpecField "Release" (defrelease ++ suffix) specfile
          patchSpec False (Just cwd) curspec newspec
          ver' <- readVersion <$> getSpecField "Version" specfile
          when (ver' /= newver) $
            editSpecField "Version" (showVersion newver) specfile
          if updated && not subpkg
            then editSpecField "Release" (defrelease ++ suffix) specfile
            else editSpecField "Release" (currel ++ suffix) specfile
          when updated $ do
            -- FIXME reset when all subpkgs updated
            unless subpkg $
              editSpecField "Release" ("0" ++ suffix) specfile
            cmd_ "rpmdev-bumpspec" ["-c", "update to" +-+ showVersion newver, specfile]
            setCurrentDirectory cwd
            rwGit <- rwGitDir
            when (rwGit && subpkg) $ do
              cmd_ "cp" ["-p", "sources", "sources.cblrpm"]
              cmd_ "sed" ["-i", "/" ++ display oldPkgId <.> "tar.gz" ++ "/d", "sources.cblrpm"]
            bringTarball newPkgId False (Just spec)
            distgit <- grepGitConfig "\\(pkgs\\|src\\)."
            when (rwGit && distgit) $ do
              cmd_ "fedpkg" ["new-sources", display newPkgId <.> "tar.gz"]
              when subpkg $ do
                shell_ $ "cat sources >>" +-+ "sources.cblrpm"
                renameFile "sources.cblrpm" "sources"
              when newrevised $
                cmd_ "git" ["add", display newPkgId <.> "cabal"]
              when revised $
                cmd_ "git" ["rm", display oldPkgId <.> "cabal"]
              cmd_ "git" ["commit", "-a", "-m", "update to" +-+ showVersion newver]
  where
    createSpecVersion :: PackageIdentifier -> String -> Bool -> Bool -> IO (FilePath, Bool)
    createSpecVersion pkgid spec revise subpkg = do
      pd <- prepare [] (streamPkgToPVS Nothing (Just pkgid)) revise
      let pkgdata' = pd { specFilename = Just spec }
          dir = display pkgid ++ if revise then ".revised" else ".orig"
      createDirectory dir
      newspec <- createSpecFile silent [] False (SpecFile spec) subpkg (Just dir) (streamPkgToPVS Nothing (Just pkgid))
      let newrevised =
            let pkgDesc = packageDesc pkgdata' in
              isJust $ lookup "x-revision" (customFieldsPD pkgDesc)
      return (newspec, newrevised)
