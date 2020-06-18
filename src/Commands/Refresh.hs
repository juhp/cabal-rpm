{-# LANGUAGE CPP #-}

-- |
-- Module      :  Commands.Refresh
-- Copyright   :  (C) 2016-2020  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
--
-- Explanation: refresh spec file to newer cabal-rpm

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Commands.Refresh (
  refresh
  ) where

import Commands.Spec (createSpecFile)
import FileUtils (withTempDirectory)
import Header (headerVersion, withSpecHead)
import PackageUtils (PackageData (..), cabal_, patchSpec, prepare)
import SysCmd (die, optionalProgram)
import Types
import Paths_cabal_rpm (version)

import SimpleCmd (cmd, cmd_, grep_)
import SimpleCmd.Git (rwGitDir)

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (unless, when)
import Data.Maybe
import Data.Version (showVersion)
import Distribution.Verbosity (silent)
import SimpleCabal (customFieldsPD)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist,
                         renameFile, setCurrentDirectory)
import System.Environment (getEnv)
--import System.Exit (exitSuccess)
import System.FilePath ((</>), (<.>))

refresh :: Bool -> PackageType -> Maybe PackageVersionSpecifier -> IO ()
refresh dryrun pkgtype mpvs = do
  pkgdata <- prepare [] mpvs True True
  case specFilename pkgdata of
    Nothing -> die "No (unique) .spec file in directory."
    Just spec -> do
      rwGit <- rwGitDir
      when rwGit $ do
        local <- cmd "git" ["diff"] :: IO String
        unless (null local) $
          putStrLn "Working dir contains local changes!"
          -- exitSuccess
      withSpecHead spec $ \ headerwords -> do
        let cblrpmver = headerVersion headerwords
        if cblrpmver == showVersion version
          then putStrLn "Packaging is up to date"
          else do
          let spectype =
                case pkgtype of
                  DefaultPkg | "--standalone" `elem` headerwords -> StandalonePkg
                  _ -> pkgtype
          subpkg <- grep_ "%{subpkgs}" spec
          let wasrevised = isJust $ lookup "x-revision" $ customFieldsPD (packageDesc pkgdata)
          oldspec <- createOldSpec subpkg cblrpmver spec
          newspec <- createSpecFile True wasrevised silent [] False False spectype (if subpkg then Just Nothing else Nothing) Nothing mpvs
          patchSpec dryrun Nothing oldspec newspec
--          setCurrentDirectory cwd
--          when rwGit $
--            cmd_ "git" ["commit", "-a", "-m", "refreshed to cabal-rpm-" ++ showVersion version]
  where
    createOldSpec :: Bool -> String -> FilePath -> IO FilePath
    createOldSpec subpkg cblrpmver spec = do
      cblrpmVersion subpkg cblrpmver
      let backup = spec <.> "cblrpm"
          backup' = backup ++ "-" ++ cblrpmver
      renameFile backup backup'
      return backup'

    cblrpmVersion :: Bool -> String -> IO ()
    cblrpmVersion subpkg crver = do
      let cblrpmver = "cabal-rpm-" ++ crver
      inpath <- optionalProgram cblrpmver
      if inpath
        then cmd_ cblrpmver ["spec"]
        else do
        home <- getEnv "HOME"
        let bindir = home </> ".cblrpm/versions/"
        haveExe <- doesFileExist $ bindir </> cblrpmver
        unless haveExe $
          withTempDirectory $ \cwd -> do
          cabal_ "unpack" [cblrpmver]
          setCurrentDirectory cblrpmver
          cabal_ "configure" []
          cabal_ "build" []
          createDirectoryIfMissing True bindir
          let bin = "dist/build/cabal-rpm" </>
                    -- this should really be <= 0.9.11 (and >= 0.7.0)
                    -- but anyway before 0.9.11 we didn't version .spec files
                    if crver == "0.9.11" then "cblrpm" else "cabal-rpm"
          cmd_ "strip" [bin]
          copyFile bin $ bindir </> cblrpmver
          setCurrentDirectory cwd
        -- Fixme 1.0.1 supports --quiet
        cmd_ (bindir </> cblrpmver) $ "spec" : ["--subpackage" | subpkg]
