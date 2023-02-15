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
import Header (headerOption, withSpecHead)
import PackageUtils (PackageData (..), RpmStage(Prep), bringTarball,
                     editSpecField, getRevisedCabal, getSpecField, latestPackage,
                     patchSpec, pkgSpecPkgData, rpmbuild)
import Stackage (defaultLTS)
import Types

import SimpleCabal (customFieldsPD, package,
                    PackageIdentifier (..), showVersion)
import SimpleCmd
import SimpleCmd.Git (gitBool, grepGitConfig, rwGitDir)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad.Extra
import Data.List
import Data.Maybe
import Distribution.Text (display)
import Distribution.Verbosity (silent)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                         removeDirectoryRecursive, renameFile)
import System.FilePath ((<.>))

-- FIXME --dryrun
-- FIXME check kerberos before new-sources
update :: Maybe PackageVersionSpecifier -> IO ()
update mpvs = do
  pkgdata <- pkgSpecPkgData [] (pvsPackage =<< mpvs)
  case specFilename pkgdata of
    Nothing -> error' "No (unique) .spec file in directory."
    Just spec -> do
      let pkgDesc = packageDesc pkgdata
          oldPkgId = package pkgDesc
          name = pkgName oldPkgId
          wasrevised = isJust $ lookup "x-revision" (customFieldsPD pkgDesc)
      (newPkgId, mstream) <-
        case mpvs of
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
            latest <- latestPackage stream name
            return (latest, stream)
          Just pvs ->
            case pvs of
              PVStreamPackage stream mpkg ->
                case mpkg of
                  Just pkg | pkg /= name -> error' "different package name"
                  _ -> do
                    latest <- latestPackage (Just stream) name
                    return (latest, Just stream)
              PVPackageId pkgid -> return (pkgid, Nothing)
              PVPackageName pkg -> do
                latest <- latestPackage Nothing pkg
                return (latest, Nothing)

      let newver = pkgVersion newPkgId
          oldver = pkgVersion oldPkgId
          updated = newver > oldver
      if newver < oldver
        then putStrLn $ "current" +-+ display oldver +-+ "is newer!"
        else do
        newrev <- getRevisedCabal newPkgId
        when (newver == oldver) $
          putStrLn "already latest version"
        when (newrev || updated) $ do
          putStrLn $ display oldPkgId +-+ "current"
          subpkg <- grep_ "%{subpkgs}" spec
          curspec <- createSpecVersion oldPkgId spec wasrevised (if subpkg then Just Nothing else Nothing)
          newspec <- createSpecVersion newPkgId spec True (if subpkg then Just mstream else Nothing)
          currel <- getSpecField "Release" spec
          let suffix = "%{?dist}"
              defrelease = "1"
          autorelease <- grep_ "^Release:        %autorelease" spec
          unless autorelease $ do
            editSpecField "Release" (defrelease ++ suffix) spec
          patchSpec False Nothing curspec newspec
          ver' <- readVersion <$> getSpecField "Version" spec
          when (ver' /= newver) $
            editSpecField "Version" (showVersion newver) spec
          unless autorelease $
            if updated && not subpkg
            then editSpecField "Release" (defrelease ++ suffix) spec
            else editSpecField "Release" (currel ++ suffix) spec
          rwGit <- rwGitDir
          autochangelog <- grep_ "^%autochangelog" spec
          when updated $ do
            -- FIXME reset when all subpkgs updated
            unless (subpkg || autorelease) $
              editSpecField "Release" ("0" ++ suffix) spec
            unless autochangelog $
              cmd_ "rpmdev-bumpspec" ["-c", "https://hackage.haskell.org/package/" ++ display newPkgId ++ "/changelog" , spec]
            when (rwGit && subpkg) $ do
              cmd_ "cp" ["-p", "sources", "sources.cblrpm"]
              cmd_ "sed" ["-i", "/" ++ display oldPkgId <.> "tar.gz" ++ "/d", "sources.cblrpm"]
            bringTarball newPkgId (Just spec)
          distgit <- grepGitConfig "\\(pkgs\\|src\\)."
          when (rwGit && distgit) $ do
            when updated $ do
              krbTicket
              cmd_ "fedpkg" ["new-sources", display newPkgId <.> "tar.gz"]
              when subpkg $ do
                shell_ $ "cat sources >>" +-+ "sources.cblrpm"
                renameFile "sources.cblrpm" "sources"
              when wasrevised $
                cmd_ "git" ["rm", display oldPkgId <.> "cabal"]
            when newrev $
              cmd_ "git" ["add", display newPkgId <.> "cabal"]
            if updated then
              cmd_ "git" ["commit", "-a", "-m",
                          if autochangelog
                          then "https://hackage.haskell.org/package/"
                               ++ display newPkgId ++ "/changelog"
                          else "update to" +-+ showVersion newver]
              else
              when newrev $
              whenM (gitBool "diff-index" ["--quiet", "HEAD"]) $
              cmd_ "git" ["commit", "-a", "-m", "revised .cabal file"]
          rpmbuild True Prep spec
  where
    -- Just Nothing is default stream
    createSpecVersion :: PackageIdentifier -> String -> Bool -> Maybe (Maybe Stream) -> IO FilePath
    createSpecVersion pkgid spec revise subpkgStream = do
      let dir = ".Cblrpm/" ++ display pkgid ++ if revise then ".revised" else ".orig"
      direxists <- doesDirectoryExist dir
      when direxists $ removeDirectoryRecursive dir
      createDirectoryIfMissing True dir
      createSpecFile False silent [] False False (SpecFile spec) subpkgStream Nothing (Just dir) (streamPkgToPVS Nothing (Just pkgid))

krbTicket :: IO ()
krbTicket = do
  krb <- words . fromMaybe "" . find ("@FEDORAPROJECT.ORG" `isInfixOf`) . lines <$> cmd "klist" ["-l"]
  if null krb
    then error' "No krb5 ticket found for FEDORAPROJECT.ORG"
    else
    when (last krb == "(Expired)") $ do
      putStrLn $ unwords krb
      cmd_ "kinit" [head krb]
      putStrLn ""
