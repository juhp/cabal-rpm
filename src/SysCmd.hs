{-# LANGUAGE CPP #-}

-- |
-- Module      :  SysCmd
-- Copyright   :  (C) 2013-2019  Jens Petersen
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
  die,
  optionalProgram,
  requireProgram,
  rpmEval
  ) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad
import Data.Maybe       (isJust, isNothing)

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
import Distribution.Simple.Utils (dieNoVerbosity)
#else
import Distribution.Simple.Utils (die)
#endif

import System.Directory (findExecutable)

import SimpleCmd (cmd)

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
die :: String -> IO a
die = dieNoVerbosity
#endif

requireProgram :: String -> IO ()
requireProgram c = do
  mavail <- findExecutable c
  when (isNothing mavail) $ die (c ++ ": command not found")

optionalProgram :: String -> IO Bool
optionalProgram c =
  isJust <$> findExecutable c

-- can't live in PackageUtils due to circular dep with Distro
rpmEval :: String -> IO (Maybe String)
rpmEval s = do
  res <- cmd "rpm" ["--eval", s]
  return $ if null res || res == s then Nothing else Just res
