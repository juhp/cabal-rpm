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
  cmd,
  cmd_,
  cmdBool,
  cmdQuiet,
  cmdSilent,
  trySystem,
  shell,
  sudo,
  yumInstall,
  (+-+)) where

import Control.Monad    (unless, void, when)
import Data.Functor     ((<$>))
import Data.List        ((\\))
import Data.Maybe       (fromMaybe, isJust, isNothing)

import Distribution.Simple.Utils (die, warn, findProgramLocation)
import Distribution.Verbosity (normal)

import System.Posix.User (getEffectiveUserID)
import System.Process (readProcess, readProcessWithExitCode, system, rawSystem)
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

cmd_ :: String -> [String] -> IO ()
cmd_ c args = do
  requireProgram c
--    putStrLn $ "cmd_:" +-+ c +-+ unwords args
  ret <- rawSystem c args
  case ret of
    ExitSuccess -> return ()
    ExitFailure n -> die ("\"" ++ c +-+ unwords args ++ "\"" +-+ "failed with status" +-+ show n)

-- hide stderr
cmdQuiet :: String -> [String] -> IO String
cmdQuiet c args = do
  requireProgram c
  (ret, out, err) <- readProcessWithExitCode c args ""
  case ret of
    ExitSuccess -> return $removeTrailingNewline out
    ExitFailure n -> die ("\"" ++ c +-+ unwords args ++ "\"" +-+ "failed with status" +-+ show n ++ "\n" ++ err)

-- hide stdout
cmdSilent :: String -> [String] -> IO ()
cmdSilent c args = do
  requireProgram c
--    putStrLn $ "cmd_:" +-+ c +-+ unwords args
  (ret, _, err) <- readProcessWithExitCode c args ""
  case ret of
    ExitSuccess -> return ()
    ExitFailure n -> die ("\"" ++ c +-+ unwords args ++ "\"" +-+ "failed with status" +-+ show n ++ "\n" ++ err)

shell :: String -> IO ()
shell c = cmd_ "sh" ["-c", c]

sudo :: String -> [String] -> IO ()
sudo c as = do
  requireProgram "sudo"
  requireProgram c
  putStrLn $ "sudo" +-+ c +-+ unwords as
  cmd_ "sudo" (c:as)

trySystem :: String -> [String] -> IO ()
trySystem c args = do
  requireProgram c
  void $ rawSystem c args

cmdBool :: String -> IO Bool
cmdBool c = do
  requireProgram $ head $ words c
  ret <- system $ c +-+ ">/dev/null"
  case ret of
    ExitSuccess -> return True
    ExitFailure _ -> return False

cmd :: FilePath -> [String] -> IO String
cmd c args = do
  requireProgram c
  removeTrailingNewline <$> readProcess c args ""

removeTrailingNewline :: String -> String
removeTrailingNewline "" = ""
removeTrailingNewline str =
  if last str == '\n'
  then init str
  else str

(+-+) :: String -> String -> String
"" +-+ s = s
s +-+ "" = s
s +-+ t = s ++ " " ++ t

yumInstall :: [String] -> Bool -> IO ()
yumInstall pkgs hard =
  unless (null pkgs) $ do
    putStrLn $ "Running repoquery" +-+ unwords pkgs
    repopkgs <- lines <$> readProcess "repoquery" (["--qf", "%{name}"] ++ pkgs) []
    if not (null (pkgs \\ repopkgs))
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
      let exec = if hard then cmd_ else trySystem
      exec (fromMaybe "yum" maybeSudo) $ maybe [] (const "yum") maybeSudo : "install" : args

showPkg :: String -> String
showPkg p = if '(' `elem` p then show p else p
