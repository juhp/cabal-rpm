-- |
-- Module      :  SysCmd
-- Copyright   :  (C) 2013-2015  Jens Petersen
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
  cmd,
  cmd_,
  cmdBool,
  cmdIgnoreErr,
  cmdQuiet,
  cmdSilent,
  optionalProgram,
  pkgInstall,
  repoquery,
  rpmInstall,
  trySystem,
  shell,
  sudo,
  (+-+)) where

import Control.Monad    (unless, void, when)
import Data.Functor     ((<$>))
import Data.List        ((\\))
import Data.Maybe       (fromMaybe, isJust, isNothing)

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,18,0)
import Distribution.Simple.Program.Find (defaultProgramSearchPath,
                                         findProgramOnSearchPath)
import Distribution.Simple.Utils (die)
#else
import Distribution.Simple.Utils (die, findProgramLocation)
#endif
import Distribution.Verbosity (normal)

import System.Posix.User (getEffectiveUserID)
import System.Process (readProcess, readProcessWithExitCode, system, rawSystem)
import System.Exit (ExitCode(..))

findProgram :: FilePath -> IO (Maybe FilePath)
findProgram =
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,18,0)
  \ prog -> findProgramOnSearchPath normal defaultProgramSearchPath prog
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,23,0)
    >>= return . fmap fst
#endif
#else
  findProgramLocation normal
#endif

requireProgram :: String -> IO ()
requireProgram c = do
  mavail <- findProgram c
  when (isNothing mavail) $ die (c ++ ": command not found")

optionalProgram :: String -> IO Bool
optionalProgram c = do
  mavail <- findProgram c
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

cmdIgnoreErr :: FilePath -> [String] -> String -> IO String
cmdIgnoreErr c args input = do
  (_exit, out, _err) <- readProcessWithExitCode c args input
  return out

removeTrailingNewline :: String -> String
removeTrailingNewline "" = ""
removeTrailingNewline str =
  if last str == '\n'
  then init str
  else str

infixr 4 +-+
(+-+) :: String -> String -> String
"" +-+ s = s
s +-+ "" = s
s +-+ t = s ++ " " ++ t

packageManager :: IO String
packageManager = do
  havednf <- optionalProgram "dnf"
  if havednf
    then return "dnf"
    else requireProgram "yum" >> return "yum"

repoquery :: [String] -> String -> IO String
repoquery args key = do
  havednf <- optionalProgram "dnf"
  let (prog, subcmd) = if havednf then ("dnf", ["repoquery", "-q"]) else ("repoquery", [])
  cmd prog (subcmd ++ args ++ [key])

pkgInstall :: [String] -> Bool -> IO ()
pkgInstall [] _ = return ()
pkgInstall pkgs hard = do
  pkginstaller <- packageManager
  putStrLn $ "Running repoquery" +-+ unwords pkgs
  repopkgs <- filter (/= "") <$> mapM (repoquery ["--qf", "%{name}"]) pkgs
  let missing = pkgs \\ repopkgs
  if not (null missing) && hard
    then error $ unwords missing +-+ "not available."
    else do
    unless (null missing) $ do
      putStrLn "Unavailable dependencies:"
      mapM_ putStrLn missing
    unless (null repopkgs) $ do
      putStrLn "Uninstalled dependencies:"
      mapM_ putStrLn repopkgs
      uid <- getEffectiveUserID
      maybeSudo <-
        if uid == 0
        then return Nothing
        else do
          havesudo <- optionalProgram "sudo"
          return $ if havesudo then Just "sudo" else Nothing
      let args = map showPkg repopkgs
      putStrLn $ "Running:" +-+ fromMaybe "" maybeSudo +-+ pkginstaller +-+ "install" +-+ unwords args
      let exec = if hard then cmd_ else trySystem
      fedora <- cmd "rpm" ["--eval", "%fedora"]
      let nogpgcheck = ["--nogpgcheck" | fedora `elem` ["22", "23"]]
      exec (fromMaybe pkginstaller maybeSudo) $ maybe [] (const pkginstaller) maybeSudo : "install" : args ++ nogpgcheck

showPkg :: String -> String
showPkg p = if '(' `elem` p then show p else p

rpmInstall :: [String] -> IO ()
rpmInstall rpms = do
  pkginstaller <- packageManager
  let (inst, arg) = if pkginstaller == "dnf" then ("dnf", "install") else ("yum", "localinstall")
  sudo inst $ ["-y", arg] ++ rpms
