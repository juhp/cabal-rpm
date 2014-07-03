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
  optionalProgram,
  requireProgram,
  runCmd,
  tryReadProcess,
  trySystem,
  shell,
  systemBool,
  yumInstall,
  (+-+)) where

import Control.Monad    (unless, void, when)
import Data.Functor     ((<$>))
import Data.List        ((\\))
import Data.Maybe       (fromMaybe, isJust, isNothing)

import Distribution.Simple.Utils (die, warn, findProgramLocation)
import Distribution.Verbosity (normal)

import System.Posix.User (getEffectiveUserID)
import System.Process (readProcess, system, rawSystem)
import System.Exit (ExitCode(..))

requireProgram :: String -> IO ()
requireProgram c = do
    mavail <- findProgramLocation normal c
    when (isNothing mavail) $ die (c ++ ": command not found")

optionalProgram :: String -> IO Bool
optionalProgram c = do
    mavail <- findProgramLocation normal c
    when (isNothing mavail) $ warn normal (c ++ ": command not found")
    return $ isJust mavail

runCmd :: String -> [String] -> IO ()
runCmd c args = do
    requireProgram c
    ret <- rawSystem c args
    case ret of
      ExitSuccess -> return ()
      ExitFailure n -> die ("\"" ++ c ++ "\"" +-+ "failed with status" +-+ show n)

shell :: String -> IO ()
shell c = runCmd "sh" ["-c", c]

trySystem :: String -> [String] -> IO ()
trySystem c args = do
    requireProgram c
    void $ rawSystem c args

systemBool :: String -> IO Bool
systemBool c = do
    requireProgram $ head $ words c
    ret <- system $ c +-+ ">/dev/null"
    case ret of
      ExitSuccess -> return True
      ExitFailure _ -> return False

tryReadProcess :: FilePath -> [String] -> IO String
tryReadProcess c args = do
  requireProgram c
  readProcess c args []

(+-+) :: String -> String -> String
"" +-+ s = s
s +-+ "" = s
s +-+ t = s ++ " " ++ t

yumInstall :: [String] -> Bool -> IO ()
yumInstall pkgs hard =
  unless (null pkgs) $ do
    putStrLn $ "Running repoquery" +-+ unwords pkgs
    repopkgs <- lines <$> readProcess "repoquery" (["--qf", "%{name}"] ++ pkgs) []
    if repopkgs /= pkgs
      then
      when hard $
        error $ unwords (pkgs \\ repopkgs) +-+ "not available."
      else do
      putStrLn "Uninstalled dependencies:"
      mapM_ putStrLn pkgs
      uid <- getEffectiveUserID
      maybeSudo <-
        if uid == 0
        then return Nothing
        else do
          havesudo <- optionalProgram "sudo"
          return $ if havesudo then Just "sudo" else Nothing
      requireProgram "yum"
      let args = map showPkg pkgs
      putStrLn $ "Running:" +-+ fromMaybe "" maybeSudo +-+ "yum install" +-+ unwords args
      let exec = if hard then runCmd else trySystem
      exec (fromMaybe "yum" maybeSudo) $ maybe [] (const "yum") maybeSudo : "install" : args

showPkg :: String -> String
showPkg p = if '(' `elem` p then show p else p
