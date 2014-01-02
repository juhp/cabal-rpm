-- |
-- Module      :  SysCmd
-- Copyright   :  (C) 2013-2014  Jens Petersen
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

module SysCmd (
  requireProgram,
  trySystem,
  tryReadProcess,
  systemBool,
  yumInstall,
  (+-+)) where

import Control.Monad    (when, unless)
import Data.Maybe       (isJust, isNothing)

import Distribution.Simple.Utils (die, warn, findProgramLocation)
import Distribution.Verbosity (normal)

import System.Posix.User (getEffectiveUserID)
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

trySystem :: String -> IO ()
trySystem cmd = do
    requireProgram $ head $ words cmd
    ret <- system cmd
    case ret of
      ExitSuccess -> return ()
      ExitFailure n -> die ("\"" ++ cmd ++ "\"" +-+ "failed with status" +-+ show n)

systemBool :: String -> IO Bool
systemBool cmd = do
    requireProgram $ head $ words cmd
    ret <- system $ cmd +-+ ">/dev/null"
    case ret of
      ExitSuccess -> return True
      ExitFailure _ -> return False

tryReadProcess :: FilePath -> [String] -> IO String
tryReadProcess cmd args = do
  requireProgram cmd
  readProcess cmd args []

(+-+) :: String -> String -> String
"" +-+ s = s
s +-+ "" = s
s +-+ t = s ++ " " ++ t

yumInstall :: [String] -> IO ()
yumInstall pkgs =
  unless (null pkgs) $ do
    putStrLn "Uninstalled dependencies:"
    mapM_ putStrLn pkgs
    uid <- getEffectiveUserID
    cmdprefix <-
      if uid == 0
      then return ""
      else do
        havesudo <- optionalProgram "sudo"
        return $ if havesudo then "sudo" else ""
    requireProgram "yum"
    let args = unwords $ map showPkg pkgs
    putStrLn $ "Running:" +-+ cmdprefix +-+ "yum install" +-+ args
    trySystem $ cmdprefix +-+ "yum install" +-+ args

showPkg :: String -> String
showPkg p = if '(' `elem` p then show p else p
