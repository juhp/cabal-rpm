-- |
-- Module      :  Distribution.Package.Rpm.Setup
-- Copyright   :  Bryan O'Sullivan 2007
--
-- Maintainer  :  Bryan O'Sullivan <bos@serpentine.com>
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
import Distribution.Simple.Setup (defaultCompilerFlavor, CompilerFlavor(..))
import Distribution.Verbosity (Verbosity(..), flagToVerbosity, normal)
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..),
                              usageInfo, getOpt')
import System.Environment (getProgName)
import System.Exit (exitWith, ExitCode (..))
import System.IO (Handle, hPutStrLn, stderr, stdout)

data RpmFlags = RpmFlags
    {
      rpmCompiler :: Maybe CompilerFlavor
    , rpmGenSpec :: Bool
    , rpmHaddock :: Bool
    , rpmHelp :: Bool
    , rpmLibProf :: Bool
    , rpmName :: Maybe String
    , rpmOptimisation :: Bool
    , rpmRelease :: Maybe String
    , rpmSplitObjs :: Bool
    , rpmTopDir :: Maybe FilePath
    , rpmVerbosity :: Verbosity
    , rpmVersion :: Maybe String
    }
    deriving (Eq, Show)

emptyRpmFlags :: RpmFlags

emptyRpmFlags = RpmFlags
    {
      rpmCompiler = defaultCompilerFlavor
    , rpmGenSpec = False
    , rpmHaddock = True
    , rpmHelp = False
    , rpmLibProf = True
    , rpmName = Nothing
    , rpmOptimisation = True
    , rpmRelease = Nothing
    , rpmSplitObjs = True
    , rpmTopDir = Nothing
    , rpmVerbosity = normal
    , rpmVersion = Nothing
    }

options :: [OptDescr (RpmFlags -> RpmFlags)]

options =
    [
      Option "" ["ghc"] (NoArg (\x -> x { rpmCompiler = Just GHC }))
             "Compile with GHC",
      Option "" ["hugs"] (NoArg (\x -> x { rpmCompiler = Just Hugs }))
             "Compile with Hugs",
      Option "" ["jhc"] (NoArg (\x -> x { rpmCompiler = Just JHC }))
             "Compile with JHC",
      Option "" ["nhc"] (NoArg (\x -> x { rpmCompiler = Just NHC }))
             "Compile with NHC",
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
      Option "" ["disable-split-objs"] (NoArg (\x -> x { rpmSplitObjs = False }))
             "Don't split object files up to save space",
      Option "" ["release"] (ReqArg (\rel x -> x { rpmRelease = Just rel }) "RELEASE")
             "Override the default package release",
      Option "" ["topdir"] (ReqArg (\path x -> x { rpmTopDir = Just path }) "TOPDIR")
             "Override the default build directory",
      Option "v" ["verbose"] (ReqArg (\verb x -> x { rpmVerbosity = flagToVerbosity (Just verb) }) "n")
             "Change build verbosity",
      Option "" ["version"] (ReqArg (\vers x -> x { rpmVersion = Just vers }) "VERSION")
             "Override the default package version"
    ]

printHelp :: Handle -> IO ()

printHelp h = do
    progName <- getProgName
    hPutStrLn h (usageInfo "Usage:" options)

parseArgs :: [String] -> IO RpmFlags

parseArgs args = do
     let (os, args', unknown, errs) = getOpt' RequireOrder options args
         opts = foldl (flip ($)) emptyRpmFlags os
     when (rpmHelp opts) $ do
       printHelp stdout
       exitWith ExitSuccess
     when (not (null errs)) $ do
       hPutStrLn stderr "Errors:"
       mapM_ (hPutStrLn stderr) errs
       exitWith (ExitFailure 1)
     when (not (null unknown)) $ do
       hPutStrLn stderr "Unrecognised options:"
       mapM_ (hPutStrLn stderr) unknown
       exitWith (ExitFailure 1)
     when (not (null args')) $ do
       hPutStrLn stderr "Unrecognised arguments:"
       mapM_ (hPutStrLn stderr) args'
       exitWith (ExitFailure 1)
     return opts
