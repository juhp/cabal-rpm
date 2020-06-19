{-# LANGUAGE CPP #-}

-- |
-- Module      :  Types
-- Copyright   :  (C) 2020  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
--
-- Explanation: types for cabal-rpm

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Types (
  Flags, -- flagList,
  LibPkgType(..),
  nullVersion,
  PackageType(..),
  PackageVersionSpecifier(..),
  pvsPackage,
  pvsStream,
  readVersion,
  RpmPackage(..),
  showRpm,
  showStream,
  Stream(..),
  streamPkgToPVS,
  unversionedPkgId,
  Verbose(..)
  ) where

import Data.Char (isDigit {--, toLower--})
import Data.List
import Data.Maybe (fromMaybe)
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,11,0))
#else
import Data.Semigroup ((<>))
#endif
import Distribution.Text (display)
import Distribution.Version (
#if (defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0))
                     Version,
                     mkVersion,
                     nullVersion
#else
                     Version(..)
#endif
                    )

import SimpleCabal (FlagName, {-- mkFlagName, --}
                    PackageIdentifier(..), PackageName)
import SimpleCmd (error')

data LibPkgType = Base | Devel | Prof | Doc | Static
  deriving Eq

instance Show LibPkgType where
  show Base = ""
  show Devel = "devel"
  show Prof = "prof"
  show Doc = "doc"
  show Static = "static"

data RpmPackage = RpmHsLib LibPkgType PackageName
                | RpmHsBin PackageName
                | RpmOther String
  deriving Eq

showRpm :: RpmPackage -> String
showRpm (RpmHsLib t n) = "ghc-" ++ display n ++ pkgSuffix t
showRpm (RpmHsBin n) = display n
showRpm (RpmOther n) = n

pkgSuffix :: LibPkgType -> String
pkgSuffix lpt =
  let rep = show lpt in
  if null rep then "" else '-' : rep

data Verbose = Quiet | Verbose
  deriving Eq

type Flags = [(FlagName, Bool)]

-- -- lifted from Distribution.Simple.Setup, since it's not exported.
-- flagList :: String -> Flags
-- flagList = map tagWithValue . words
--   where tagWithValue ('-':name) = (mkFlagName (map toLower name), False)
--         tagWithValue name       = (mkFlagName (map toLower name), True)

data Stream = LTS Int | LatestLTS | Nightly String | LatestNightly | Hackage
  deriving (Eq, Ord)

showStream :: Stream -> String
showStream LatestNightly = "nightly"
showStream LatestLTS = "lts"
showStream (Nightly date) = "nightly-" <> date
showStream (LTS ver) = "lts-" <> show ver
showStream Hackage = "hackage"

instance Read Stream where
  readsPrec _ "hackage" = [(Hackage,"")]
  readsPrec _ "nightly" = [(LatestNightly,"")]
  readsPrec _ "lts" = [(LatestLTS,"")]
  readsPrec _ input | "nightly-" `isPrefixOf` input =
    let (date,rest) = span (\ c -> isDigit c || c == '-') $ removePrefix "nightly-" input in
       [(Nightly date,rest)]
  readsPrec _ input | "lts-" `isPrefixOf` input =
    let (ver,rest) = span isDigit $ removePrefix "lts-" input in
      [(LTS (read ver),rest)]
  readsPrec _ _ = []

removePrefix :: String -> String-> String
removePrefix pref orig = fromMaybe orig (stripPrefix pref orig)

data PackageType = DefaultPkg | BinaryPkg | StandalonePkg | SpecFile FilePath
  deriving Eq

readVersion :: String -> Version
readVersion = mkVersion . parseVer
  where
    parseVer :: String -> [Int]
    parseVer cs =
      let vs = filter (/= ".") $ groupBy (\ c c' -> c /= '.' && c' /= '.') cs
      in map read vs

unversionedPkgId :: PackageName -> PackageIdentifier
unversionedPkgId pn = PackageIdentifier pn nullVersion

#if (defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0))
#else
mkVersion :: [Int] -> Version
mkVersion is = Version is []

nullVersion :: Version
nullVersion = mkVersion []
#endif

data PackageVersionSpecifier = PVStreamPackage Stream (Maybe PackageName)
                             | PVPackageId PackageIdentifier
                             | PVPackageName PackageName

pvsStream :: PackageVersionSpecifier -> Maybe Stream
pvsStream (PVStreamPackage s _) = Just s
pvsStream (PVPackageId _) = Nothing
pvsStream (PVPackageName _) = Nothing

pvsPackage :: PackageVersionSpecifier -> Maybe PackageName
pvsPackage (PVStreamPackage _ mp) = mp
pvsPackage (PVPackageId pkgid) = Just $ pkgName pkgid
pvsPackage (PVPackageName pkg) = Just pkg

streamPkgToPVS :: Maybe Stream -> Maybe PackageIdentifier
               -> Maybe PackageVersionSpecifier
streamPkgToPVS Nothing Nothing = Nothing
streamPkgToPVS Nothing (Just pkgid) =
  Just $ if pkgVersion pkgid == nullVersion
         then PVPackageName (pkgName pkgid)
         else PVPackageId pkgid
streamPkgToPVS (Just s) Nothing = Just $ PVStreamPackage s Nothing
streamPkgToPVS (Just s) (Just p) | pkgVersion p == nullVersion =
                                     Just $ PVStreamPackage s $ Just (pkgName p)
streamPkgToPVS (Just _) (Just _) =
  error' "cannot specify stream and package version"
