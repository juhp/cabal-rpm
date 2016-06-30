-- |
-- Module      :  Setup
-- Copyright   :  (C) 2007-2008  Bryan O'Sullivan
--                (C) 2012-2014  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Command line option processing for building RPM
-- packages.

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Setup (
      RpmFlags(..)
    , parseArgs
    , quiet
    ) where

import Control.Monad (unless, when)
import Data.Char     (toLower)
import Data.Maybe    (listToMaybe, fromMaybe)
import Data.Version  (showVersion)

import Distribution.Compiler           (CompilerId)
import Distribution.Text               (simpleParse)
import Distribution.PackageDescription (FlagName (..))
import Distribution.ReadE              (readEOrFail)
import Distribution.Verbosity          (Verbosity, flagToVerbosity, normal,
                                        silent)

import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..),
                              getOpt', usageInfo)
import System.Environment    (getProgName)
import System.Exit           (ExitCode (..), exitSuccess, exitWith)
import System.IO             (Handle, hPutStrLn, stderr, stdout)

import Paths_cabal_rpm       (version)
import SysCmd                ((+-+))

data RpmFlags = RpmFlags
    { rpmConfigurationsFlags :: [(FlagName, Bool)]
    , rpmForce               :: Bool
    , rpmHelp                :: Bool
    , rpmBinary              :: Bool
    , rpmRelease             :: Maybe String
    , rpmCompilerId          :: Maybe CompilerId
    , rpmVerbosity           :: Verbosity
    , rpmVersion             :: Bool
    }
    deriving (Eq, Show)

emptyRpmFlags :: RpmFlags
emptyRpmFlags = RpmFlags
    { rpmConfigurationsFlags = []
    , rpmForce = False
    , rpmHelp = False
    , rpmBinary = False
    , rpmRelease = Nothing
    , rpmCompilerId = Nothing
    , rpmVerbosity = normal
    , rpmVersion = False
    }

quiet :: RpmFlags
quiet = emptyRpmFlags {rpmVerbosity = silent}

options :: [OptDescr (RpmFlags -> RpmFlags)]
options =
    [
      Option "h?" ["help"] (NoArg (\x -> x { rpmHelp = True }))
             "Show this help text",
      Option "b" ["binary"] (NoArg (\x -> x { rpmBinary = True }))
             "Force Haskell package name to be base package name",
      Option "f" ["flags"] (ReqArg (\flags x -> x { rpmConfigurationsFlags = rpmConfigurationsFlags x ++ flagList flags }) "FLAGS")
             "Set given flags in Cabal conditionals",
      Option "" ["force"] (NoArg (\x -> x { rpmForce = True }))
             "Overwrite existing spec file.",
      Option "" ["release"] (ReqArg (\rel x -> x { rpmRelease = Just rel }) "RELEASE")
             "Override the default package release",
      Option "" ["compiler"] (ReqArg (\cid x -> x { rpmCompilerId = Just (parseCompilerId cid) }) "COMPILER-ID")
             "Finalize Cabal files targetting the given compiler version",
      Option "v" ["verbose"] (ReqArg (\verb x -> x { rpmVerbosity = readEOrFail flagToVerbosity verb }) "n")
             "Change build verbosity",
      Option "V" ["version"] (NoArg (\x -> x { rpmVersion = True }))
             "Show version number"
    ]

-- Lifted from Distribution.Simple.Setup, since it's not exported.
flagList :: String -> [(FlagName, Bool)]
flagList = map tagWithValue . words
  where tagWithValue ('-':name) = (FlagName (map toLower name), False)
        tagWithValue name       = (FlagName (map toLower name), True)

printHelp :: Handle -> IO ()
printHelp h = do
  progName <- getProgName
  let info = "Usage: " ++ progName ++ " [OPTION]... COMMAND [PATH|PKG|PKG-VERSION]\n"
             ++ "\n"
             ++ "PATH can be a .spec file, .cabal file, or pkg dir.\n"
             ++ "\n"
             ++ "Commands:\n"
             ++ "  spec\t\t- generate a spec file\n"
             ++ "  srpm\t\t- generate a src rpm file\n"
             ++ "  prep\t\t- unpack source\n"
             ++ "  local\t\t- build rpm package locally\n"
             ++ "  builddep\t- install dependencies\n"
             ++ "  install\t- install packages recursively\n"
             ++ "  depends\t- list Cabal depends\n"
             ++ "  requires\t- list package buildrequires\n"
             ++ "  missingdeps\t- list missing buildrequires\n"
             ++ "  diff\t\t- diff current spec file\n"
             ++ "  update\t- update spec file package to latest version\n"
--             ++ "  mock\t\t- mock build package\n"
             ++ "\n"
             ++ "Options:"
  hPutStrLn h (usageInfo info options)

parseCompilerId :: String -> CompilerId
parseCompilerId x = fromMaybe err (simpleParse x)
  where err = error (show x ++ " is not a valid compiler id")

parseArgs :: [String] -> IO (RpmFlags, String, Maybe String)
parseArgs args = do
  let (os, args', unknown, errs) = getOpt' Permute options args
      opts = foldl (flip ($)) emptyRpmFlags os
  when (rpmHelp opts) $ do
    printHelp stdout
    exitSuccess
  when (rpmVersion opts) $ do
    putStrLn $ showVersion version
    exitSuccess
  unless (null errs) $
    error $ unlines errs
  unless (null unknown) $
    error $ "Unrecognised options:" +-+ unwords unknown
  when (null args') $ do
    printHelp stderr
    exitWith (ExitFailure 1)
  when (notElem (head args') ["builddep", "depends", "diff", "install", "missingdeps", "prep", "requires", "spec", "srpm", "build", "local", "rpm", "update", "refresh"]) $ do
    hPutStrLn stderr $ "Unknown command:" +-+ head args'
    printHelp stderr
    exitWith (ExitFailure 1)
  when (length args' > 2) $
    error $ "Too many arguments:" +-+ unwords args'
  return (opts, head args', listToMaybe $ tail args')
