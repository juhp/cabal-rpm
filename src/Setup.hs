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
    ) where

import Control.Monad (unless, when)
import Data.Char     (toLower)
import Data.Version  (showVersion)

import Distribution.PackageDescription (FlagName (..))
import Distribution.ReadE              (readEOrFail)
import Distribution.Verbosity          (Verbosity, flagToVerbosity, normal)

import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..),
                              getOpt', usageInfo)
import System.Environment    (getProgName)
import System.Exit           (ExitCode (..), exitSuccess, exitWith)
import System.IO             (Handle, hPutStr, hPutStrLn, stderr, stdout)

import Paths_cabal_rpm       (version)

data RpmFlags = RpmFlags
    { rpmConfigurationsFlags :: [(FlagName, Bool)]
    , rpmForce               :: Bool
    , rpmHelp                :: Bool
    , rpmLibrary             :: Bool
    , rpmRelease             :: Maybe String
    , rpmVerbosity           :: Verbosity
    , rpmVersion             :: Bool
    }
    deriving (Eq, Show)

emptyRpmFlags :: RpmFlags
emptyRpmFlags = RpmFlags
    { rpmConfigurationsFlags = []
    , rpmForce = False
    , rpmHelp = False
    , rpmLibrary = False
    , rpmRelease = Nothing
    , rpmVerbosity = normal
    , rpmVersion = False
    }

options :: [OptDescr (RpmFlags -> RpmFlags)]

options =
    [
      Option "h?" ["help"] (NoArg (\x -> x { rpmHelp = True }))
             "Show this help text",
      Option "l" ["library"] (NoArg (\x -> x { rpmLibrary = True }))
             "Force package to be a Library ignoring executables",
      Option "f" ["flags"] (ReqArg (\flags x -> x { rpmConfigurationsFlags = rpmConfigurationsFlags x ++ flagList flags }) "FLAGS")
             "Set given flags in Cabal conditionals",
      Option "" ["force"] (NoArg (\x -> x { rpmForce = True }))
             "Overwrite existing spec file.",
      Option "" ["release"] (ReqArg (\rel x -> x { rpmRelease = Just rel }) "RELEASE")
             "Override the default package release",
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
            ++ "PATH can be to a .spec or .cabal file, pkg dir, or tarball.\n"
            ++ "\n"
            ++ "Commands:\n"
            ++ "  spec\t\t- generate a spec file\n"
            ++ "  srpm\t\t- generate a src rpm file\n"
            ++ "  prep\t\t- unpack source\n"
            ++ "  local\t\t- build rpm package locally\n"
            ++ "  builddep\t- install dependencies\n"
            ++ "  install\t- user install package\n"
            ++ "  depends\t- list Cabal depends\n"
            ++ "  requires\t- list package buildrequires\n"
            ++ "  missingdeps\t- list missing buildrequires\n"
            ++ "  diff\t\t- diff current spec file\n"
--             ++ "  mock\t\t- mock build package\n"
            ++ "\n"
            ++ "Options:"
    hPutStrLn h (usageInfo info options)

parseArgs :: [String] -> IO (RpmFlags, [String])
parseArgs args = do
     let (os, args', unknown, errs) = getOpt' Permute options args
         opts = foldl (flip ($)) emptyRpmFlags os
     when (rpmHelp opts) $ do
       printHelp stdout
       exitSuccess
     when (rpmVersion opts) $ do
       putStrLn $ showVersion version
       exitSuccess
     unless (null errs) $ do
       hPutStrLn stderr "Error:"
       mapM_ (hPutStrLn stderr) errs
       exitWith (ExitFailure 1)
     unless (null unknown) $ do
       hPutStr stderr "Unrecognised options: "
       hPutStrLn stderr $ unwords unknown
       exitWith (ExitFailure 1)
     when (null args' || notElem (head args') ["builddep", "depends", "diff", "install", "missingdeps", "prep", "requires", "spec", "srpm", "local", "rpm"]) $ do
       printHelp stderr
       exitWith (ExitFailure 1)
     when (length args' > 2) $ do
       hPutStr stderr "Too many arguments: "
       hPutStrLn stderr $ unwords args'
       exitWith (ExitFailure 1)
     return (opts, args')
