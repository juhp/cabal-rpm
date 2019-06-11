{-# LANGUAGE CPP #-}

-- |
-- Module      :  Types
-- Copyright   :  (C) 2019  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
--
-- Explanation: types for cabal-rpm

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Types (
  Flags, flagList, -- mkFlagName, unFlagName,
  Package,
  PackageType(..),
  Stream(..),
  Verbose(..)
  ) where

import Data.Char (isDigit, toLower)
import Data.List
import Data.Maybe (fromMaybe)
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,11,0))
#else
import Data.Semigroup ((<>))
#endif
import Distribution.PackageDescription (
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
  FlagName,
  mkFlagName,
#else
  FlagName (..)
#endif
  )

type Package = String

data Verbose = Quiet | Verbose
  deriving Eq

type Flags = [(FlagName, Bool)]

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
#else
mkFlagName :: String -> FlagName
mkFlagName = FlagName
#endif

-- lifted from Distribution.Simple.Setup, since it's not exported.
flagList :: String -> [(FlagName, Bool)]
flagList = map tagWithValue . words
  where tagWithValue ('-':name) = (mkFlagName (map toLower name), False)
        tagWithValue name       = (mkFlagName (map toLower name), True)

data Stream = LatestNightly | LatestLTS | LTS String | Nightly String | Hackage

instance Show Stream where
  show LatestNightly = "nightly"
  show LatestLTS = "lts"
  show (Nightly date) = "nightly-" <> date
  show (LTS ver) = "lts-" <> ver
  show Hackage = "hackage"

instance Read Stream where
  readsPrec _ "nightly" = [(LatestNightly,"")]
  readsPrec _ "lts" = [(LatestLTS,"")]
  readsPrec _ input | "nightly-" `isPrefixOf` input =
    let (date,rest) = span (\ c -> isDigit c || c == '-') $ removePrefix "nightly-" input in
       [(Nightly date,rest)]
  readsPrec _ input | "lts-" `isPrefixOf` input =
    let (ver,rest) = span (\ c -> isDigit c || c == '.') $ removePrefix "lts-" input in
      [(LTS ver,rest)]
  readsPrec _ _ = []

removePrefix :: String -> String-> String
removePrefix pref orig = fromMaybe orig (stripPrefix pref orig)


data PackageType = DefaultPkg | BinaryPkg | StandalonePkg | SpecFile FilePath
  deriving Eq
