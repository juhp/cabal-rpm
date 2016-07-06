-- |
-- Module      :  Distro
-- Copyright   :  (C) 2016  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
--
-- Types and utility functions to represent different RPM-based distributions.

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Distro (
    detectDistro, parseDistroName, readDistroName, Distro(..)
  ) where

import SysCmd (cmd)
import Data.Maybe (fromMaybe)
import Data.Char (toLower)

data Distro = Fedora | RHEL5 | SUSE deriving (Show, Eq)

detectDistro :: IO Distro
detectDistro = do
  suseVersion <- cmd "rpm" ["--eval", "%{?suse_version}"]
  if null suseVersion then do
    dist <- cmd "rpm" ["--eval", "%{?dist}"]
    -- RHEL5 does not have macros.dist
    return $ if null dist || dist == ".el5" then RHEL5 else Fedora
    else return SUSE

parseDistroName :: String -> Maybe Distro
parseDistroName x = lookup (map toLower x) known
  where
    known = [ ("suse", SUSE)
            , ("fedora", Fedora)
            , ("rhel5", RHEL5)
            ]

readDistroName :: String -> Distro
readDistroName s = fromMaybe (error $ "unrecognized distribution name " ++ show s) (parseDistroName s)
