-- |
-- Module      :  Distro
-- Copyright   :  (C) 2016, 2018  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
--
-- Types and utility functions to represent different RPM-based distributions.

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Distro (defaultRelease,
               detectDistro,
               parseDistroName,
               readDistroName,
               Distro(..)) where

import SysCmd (rpmEval, rpmMacroDefined)
import Data.Maybe (fromMaybe, isNothing)
import Data.Char (toLower)

data Distro = Fedora | RHEL5 | SUSE deriving (Show, Eq)

detectDistro :: IO Distro
detectDistro = do
  suse <- rpmMacroDefined "suse_version"
  if suse then return SUSE
    else do
    dist <- rpmEval "%{?dist}"
    -- RHEL5 does not have macros.dist
    return $ if isNothing dist || dist == Just ".el5" then RHEL5 else Fedora

parseDistroName :: String -> Maybe Distro
parseDistroName x = lookup (map toLower x) known
  where
    known = [ ("suse", SUSE)
            , ("fedora", Fedora)
            , ("rhel5", RHEL5)
            ]

readDistroName :: String -> Distro
readDistroName s = fromMaybe (error $ "unrecognized distribution name " ++ show s) (parseDistroName s)

defaultRelease :: Distro -> String
defaultRelease distro =
    if distro == SUSE then "0" else "1"
