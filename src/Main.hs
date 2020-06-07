{-# LANGUAGE CPP #-}

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
import Options.Applicative (maybeReader)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)

import Commands.BuildDep (builddep)
import Commands.Depends (depends, Depends (..))
import Commands.Diff (diff)
import Commands.Install (install)
import Commands.Refresh (refresh)
import Commands.RpmBuild (rpmBuild_)
import Commands.Spec (createSpecFile_)
import Commands.Update (update)

import PackageUtils (RpmStage (..))
import Paths_cabal_rpm (version)
import Types

import SimpleCabal (PackageIdentifier(..))
import SimpleCmdArgs

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  simpleCmdArgs (Just version) "Cabal-rpm tool"
    "RPM package tool for Haskell Stackage/Hackage packages" $
    subcommands
    [ Subcommand "spec" "Generate a spec file" $
      createSpecFile_ <$> quietOpt <*> flags <*> testsuite <*> force <*> pkgtype <*> fmap toSubpkgStream subpackage <*> pkgVerSpecifier
    , Subcommand "srpm" "Generate an srpm" $
      rpmBuild_ Source <$> flags <*> pkgtype <*> subpackage <*> pkgVerSpecifier
    , Subcommand "prep" "Unpack source" $
      rpmBuild_ Prep <$> flags <*> pkgtype <*> subpackage <*> pkgVerSpecifier
    , Subcommand "local" "Build rpm package locally" $
      rpmBuild_ Binary <$> flags <*> pkgtype <*> subpackage <*> pkgVerSpecifier
    , Subcommand "build" "Alias for 'local' - builds rpm locally" $
      rpmBuild_ Binary <$> flags <*> pkgtype <*> subpackage <*> pkgVerSpecifier
    , Subcommand "builddep" "Install build dependencies with dnf" $
      builddep <$> flags <*> pkgVerSpecifier
    , Subcommand "install" "Build and install recursively" $
      install <$> flags <*> pkgtype <*> subpackage <*> pkgVerSpecifier
    -- should be (optional versionArg) not pkgid
    , Subcommand "diff" "Diff with pristine generated spec file" $
      diff <$> flags <*> pkgtype <*> pkgVerSpecifier
    , Subcommand "depends" "List Haskell dependencies" $
      depends Depends <$> flags <*> pkgVerSpecifier
    , Subcommand "requires" "List buildrequires for package" $
      depends Requires <$> flags <*> pkgVerSpecifier
    , Subcommand "missingdeps" "List dependencies not available" $
      depends Missing <$> flags <*> pkgVerSpecifier
    -- should be just Maybe PackageName
    , Subcommand "refresh" "Refresh spec file to latest packaging" $
      refresh <$> dryrun <*> pkgtype <*> pkgVerSpecifier
    , Subcommand "update" "Update package to latest version" $
      update <$> pkgVerSpecifier
    ]
  where
    pkgId :: Parser (Maybe PackageIdentifier)
    pkgId = optional (argumentWith (maybeReader simpleParse) "PKG[VER]")

    stream :: Parser (Maybe Stream)
    stream = optional (optionWith auto 's' "stream" "STREAM" "Stackage stream or Hackage")

    flags :: Parser Flags
    flags = optionalWith auto 'f' "flag" "[(String,Bool)]" "Set or disable Cabal flags" []

    testsuite :: Parser Bool
    testsuite = switchWith 'T' "tests" "Force enabling the test-suite (even if deps missing)"

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

    pkgVerSpecifier :: Parser (Maybe PackageVersionSpecifier)
    pkgVerSpecifier = streamPkgToPVS <$> stream <*> pkgId

    toSubpkgStream :: Bool -> Maybe (Maybe Stream)
    toSubpkgStream False = Nothing
    toSubpkgStream True = Just Nothing
