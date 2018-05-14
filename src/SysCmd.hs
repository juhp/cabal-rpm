-- |
-- Module      :  SysCmd
-- Copyright   :  (C) 2013-2016  Jens Petersen
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
  cmdMaybe,
  cmdQuiet,
  cmdSilent,
  grep_,
  notNull,
  optionalProgram,
  requireProgram,
  trySystem,
  shell,
  sudo,
  (+-+)) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad    (void, when)
import Data.Maybe       (isJust, isNothing)

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,18,0)
import Distribution.Simple.Program.Find (defaultProgramSearchPath,
                                         findProgramOnSearchPath)
import Distribution.Simple.Utils (die)
#else
import Distribution.Simple.Utils (die, findProgramLocation)
#endif
import Distribution.Verbosity (normal)

import System.Process (readProcess, readProcessWithExitCode, rawSystem)
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
optionalProgram c =
  isJust <$> findProgram c

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
    ExitSuccess -> return $ removeTrailingNewline out
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
sudo c args = do
  requireProgram "sudo"
  requireProgram c
  putStrLn $ "sudo" +-+ c +-+ unwords args
  cmd_ "sudo" (c:args)

trySystem :: String -> [String] -> IO ()
trySystem c args = do
  requireProgram c
  void $ rawSystem c args

cmdBool :: String -> [String] -> IO Bool
cmdBool c args = do
  requireProgram c
  (ret, _out, _err) <- readProcessWithExitCode c args ""
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

cmdMaybe :: String -> [String] -> IO (Maybe String)
cmdMaybe c args = do
  (ret, out, _err) <- readProcessWithExitCode c args ""
  case ret of
    ExitSuccess -> return $ Just $ removeTrailingNewline out
    ExitFailure _ -> return Nothing

-- grep :: String -> FilePath -> IO [String]
-- grep pat file =
--   lines <$> cmd "grep" [pat, file]

grep_ :: String -> FilePath -> IO Bool
grep_ pat file =
  cmdBool "grep" ["-q", pat, file]

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
s +-+ t | last s == ' ' = s ++ t
        | head t == ' ' = s ++ t
s +-+ t = s ++ " " ++ t

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
notNull :: Foldable t => t a -> Bool
#else
notNull :: [a] -> Bool
#endif
notNull = not . null
