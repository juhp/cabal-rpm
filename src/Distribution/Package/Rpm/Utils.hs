-- |
-- Module      :  Distribution.Package.Rpm.Utils
-- Copyright   :  Jens Petersen 2013
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Command line option processing for building RPM
-- packages.

-- This software may be used and distributed according to the terms of
-- the GNU General Public License, incorporated herein by reference.

module Distribution.Package.Rpm.Utils (
  requireProgram,
  trySystem,
  tryReadProcess,
  optionalSudo,
  (+-+)) where

import Control.Monad    (when)
import Data.Maybe       (isJust, isNothing)

import Distribution.Simple.Utils (die, warn, findProgramLocation)
import Distribution.Verbosity (normal)

import System.Process (readProcess, system)
import System.Exit (ExitCode(..))

requireProgram :: String -> IO ()
requireProgram cmd = do
    mavail <- findProgramLocation normal cmd
    when (isNothing mavail) $ die (cmd ++ ": command not found")

optionalProgram :: String -> IO Bool
optionalProgram cmd = do
    mavail <- findProgramLocation normal cmd
    when (isNothing mavail) $ warn normal (cmd ++ ": command not found")
    return $ isJust mavail

optionalSudo :: String -> IO ()
optionalSudo cmd = do
    havesudo <- optionalProgram "sudo"
    when havesudo $ do
      let argv = words cmd
          cmd0 =  head argv 
      mavail <- findProgramLocation normal cmd0
      case mavail of
        Nothing -> warn normal $ cmd0 ++ ": command not found"
        Just _ -> do
          ret <- system $ "sudo" +-+ cmd
          case ret of
            ExitSuccess -> return ()
            ExitFailure n -> warn normal ("\"" ++ cmd ++ "\"" +-+ "failed with status" +-+ show n)

trySystem :: String -> IO ()
trySystem cmd = do
    requireProgram $ head $ words cmd
    ret <- system cmd
    case ret of
      ExitSuccess -> return ()
      ExitFailure n -> die ("\"" ++ cmd ++ "\"" +-+ "failed with status" +-+ show n)

tryReadProcess :: FilePath -> [String] -> IO String
tryReadProcess cmd args = do
  requireProgram cmd
  readProcess cmd args []

(+-+) :: String -> String -> String
"" +-+ s = s
s +-+ "" = s
s +-+ t = s ++ " " ++ t

