-- |
-- Module      :  Main
-- Copyright   :  (C) 2007  Bryan O'Sullivan
--                (C) 2012-2018  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Main entry point for building RPM packages.

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Main where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,13,0))
#else
import Control.Applicative ((<|>)
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
                         , (<$>), (<*>)
#endif
                           )
#endif
import Distribution.Text (simpleParse)
import Distribution.Verbosity (normal, silent)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)

import Commands.BuildDep (builddep)
import Commands.Depends (depends, Depends (..))
import Commands.Diff (diff)
import Commands.Install (install)
import Commands.Refresh (refresh)
import Commands.RpmBuild (rpmBuild_)
import Commands.Spec (createSpecFile_)
import Commands.Update (update)

import Options.Applicative (maybeReader)
import PackageUtils (RpmStage (..))
import Paths_cabal_rpm       (version)
import Types

import SimpleCabal (PackageIdentifier)
import SimpleCmdArgs

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  simpleCmdArgs (Just version) "Cabal-rpm tool"
    "RPM package tool for Haskell Stackage/Hackage packages" $
    subcommands
    [ Subcommand "spec" "Generate a spec file" $
      createSpecFile_ Nothing <$> quietOpt <*> flags <*> force <*> pkgtype <*> subpackage <*> stream <*> pkgId
    , Subcommand "srpm" "Generate an srpm" $
      rpmBuild_ Source <$> flags <*> pkgtype <*> subpackage <*> stream <*> pkgId
    , Subcommand "prep" "Unpack source" $
      rpmBuild_ Prep <$> flags <*> pkgtype <*> subpackage <*> stream <*> pkgId
    , Subcommand "local" "Build rpm package locally" $
      rpmBuild_ Binary <$> flags <*> pkgtype <*> subpackage <*> stream <*> pkgId
    , Subcommand "build" "Alias for 'local' - builds rpm locally" $
      rpmBuild_ Binary <$> flags <*> pkgtype <*> subpackage <*> stream <*> pkgId
    , Subcommand "builddep" "Install build dependencies with dnf" $
      builddep <$> flags <*> stream <*> pkgId
    , Subcommand "install" "Build and install recursively" $
      install <$> flags <*> pkgtype <*> subpackage <*> stream <*> pkgId
    -- should be (optional versionArg) not pkgid
    , Subcommand "diff" "Diff with pristine generated spec file" $
      diff <$> flags <*> pkgtype <*> stream <*> pkgId
    , Subcommand "depends" "List Haskell dependencies" $
      depends Depends <$> flags <*> stream <*> pkgId
    , Subcommand "requires" "List buildrequires for package" $
      depends Requires <$> flags <*> stream <*> pkgId
    , Subcommand "missingdeps" "List dependencies not available" $
      depends Missing <$> flags <*> stream <*> pkgId
    -- should be just Maybe PackageName
    , Subcommand "refresh" "Refresh spec file to lastest packaging" $
      refresh <$> dryrun <*> pkgtype <*> stream <*> pkgId
    , Subcommand "update" "Update package to latest version" $
      update <$> stream <*> optional versionArg
    ]
  where
    pkgId :: Parser (Maybe PackageIdentifier)
    pkgId = optional (argumentWith (maybeReader simpleParse) "PKG[VER]")

    stream :: Parser Stream
    stream = optionalWith auto 's' "stream" "STREAM" "Stackage stream or Hackage" (LTS "13")

    flags :: Parser Flags
    flags = optionalWith auto 'f' "flag" "[(String,Bool)]" "Set or disable Cabal flags" []

    force :: Parser Bool
    force = switchWith 'F' "force" "Force overwriting existing of any .spec file"

    dryrun = switchWith 'n' "dry-run" "Just show patch"

    -- quietOpt :: Parser Verbosity
    quietOpt = flagWith normal silent 'q' "quiet" "Silence Cabal"

    pkgtype :: Parser PackageType
    pkgtype =
      flagWith' StandalonePkg 't' "standalone" "Create a standalone package that uses cabal-install to build and install" <|>
      flagWith DefaultPkg BinaryPkg 'b' "binary" "Make the base package name to be the Haskell package name"

    subpackage :: Parser Bool
    subpackage = switchWith 'S' "subpackage" "Subpackage missing Haskell dependencies"

    -- FIXME: use Version
    versionArg :: Parser String
    versionArg = strArg "VERSION"
