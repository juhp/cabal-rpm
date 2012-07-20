-- |
-- Module      :  Distribution.Package.Rpm.Setup
-- Copyright   :  Bryan O'Sullivan 2007, 2008
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Command line option processing for building RPM
-- packages.

-- This software may be used and distributed according to the terms of
-- the GNU General Public License, incorporated herein by reference.

module Distribution.Package.Rpm.Setup (
      RpmFlags(..)
    , parseArgs
    ) where

import Control.Monad (when)
import Data.Char (toLower)
-- import Distribution.Simple.Setup
import Distribution.PackageDescription (FlagName(..))
import Distribution.ReadE (readEOrFail)
import Distribution.Verbosity (Verbosity, normal, flagToVerbosity)
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..),
                              usageInfo, getOpt')
import System.Environment (getProgName)
import System.Exit (exitWith, ExitCode (..))
import System.IO (Handle, hPutStrLn, hPutStr, stderr, stdout)

data RpmFlags = RpmFlags
    { rpmConfigurationsFlags :: [(FlagName, Bool)]
    , rpmGenSpec :: Bool
    , rpmHaddock :: Bool
    , rpmHelp :: Bool
    , rpmLibProf :: Bool
    , rpmName :: Maybe String
    , rpmOptimisation :: Bool
    , rpmRelease :: Maybe String
    , rpmTopDir :: Maybe FilePath
    , rpmVerbosity :: Verbosity
    , rpmVersion :: Maybe String
    }
    deriving (Eq, Show)

emptyRpmFlags :: RpmFlags

emptyRpmFlags = RpmFlags
    { rpmConfigurationsFlags = []
    , rpmGenSpec = False
    , rpmHaddock = True
    , rpmHelp = False
    , rpmLibProf = True
    , rpmName = Nothing
    , rpmOptimisation = True
    , rpmRelease = Nothing
    , rpmTopDir = Nothing
    , rpmVerbosity = normal
    , rpmVersion = Nothing
    }

options :: [OptDescr (RpmFlags -> RpmFlags)]

options =
    [
      Option "" ["gen-spec"] (NoArg (\x -> x { rpmGenSpec = True }))
             "Generate a spec file, nothing more",
      Option "h?" ["help"] (NoArg (\x -> x { rpmHelp = True }))
             "Show this help text",
      Option "" ["name"] (ReqArg (\name x -> x { rpmName = Just name }) "NAME")
             "Override the default package name",
      Option "" ["disable-haddock"] (NoArg (\x -> x { rpmHaddock = False }))
             "Don't generate API docs",
      Option "" ["disable-library-profiling"] (NoArg (\x -> x { rpmLibProf = False }))
             "Don't generate profiling libraries",
      Option "" ["disable-optimization"] (NoArg (\x -> x { rpmOptimisation = False }))
             "Don't generate optimised code",
      Option "f" ["flags"] (ReqArg (\flags x -> x { rpmConfigurationsFlags = rpmConfigurationsFlags x ++ flagList flags }) "FLAGS")
             "Set given flags in Cabal conditionals",
      Option "" ["release"] (ReqArg (\rel x -> x { rpmRelease = Just rel }) "RELEASE")
             "Override the default package release",
      Option "" ["topdir"] (ReqArg (\path x -> x { rpmTopDir = Just path }) "TOPDIR")
             "Override the default build directory",
      Option "v" ["verbose"] (ReqArg (\verb x -> x { rpmVerbosity = readEOrFail flagToVerbosity verb }) "n")
             "Change build verbosity",
      Option "" ["version"] (ReqArg (\vers x -> x { rpmVersion = Just vers }) "VERSION")
             "Override the default package version"
    ]

-- Lifted from Distribution.Simple.Setup, since it's not exported.
flagList :: String -> [(FlagName, Bool)]
flagList = map tagWithValue . words
  where tagWithValue ('-':name) = (FlagName (map toLower name), False)
        tagWithValue name       = (FlagName (map toLower name), True)

printHelp :: Handle -> IO ()

printHelp h = do
    progName <- getProgName
    let info = "Usage: " ++ progName ++ " [OPTION]... [PKGPATH]\n" ++
               "Generate a RPM .spec file from " ++
               "a .cabal file, dir, or package name\n"
    hPutStrLn h (usageInfo info options)

parseArgs :: [String] -> IO (RpmFlags, [String])
parseArgs args = do
     let (os, args', unknown, errs) = getOpt' RequireOrder options args
         opts = foldl (flip ($)) emptyRpmFlags os
     when (rpmHelp opts) $ do
       printHelp stdout
       exitWith ExitSuccess
     when (not (null errs)) $ do
       hPutStrLn stderr "Error:"
       mapM_ (hPutStrLn stderr) errs
       exitWith (ExitFailure 1)
     when (not (null unknown)) $ do
       hPutStr stderr "Unrecognised options: "
       hPutStrLn stderr $ unwords unknown
       exitWith (ExitFailure 1)
     when ((length args') > 1) $ do
       hPutStr stderr "Too many arguments: "
       hPutStrLn stderr $ unwords args'
       exitWith (ExitFailure 1)
     return (opts, args')
