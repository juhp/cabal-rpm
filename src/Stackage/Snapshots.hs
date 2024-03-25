{-# LANGUAGE CPP, OverloadedStrings #-}

module Stackage.Snapshots (
  latestMajorSnapshot,
  latestLTS,
  latestLtsSnapshot,
  resolveMajor
  )
where

import Data.Aeson
import qualified Data.Text as T
import Network.HTTP.Query
import SimpleCmd (error')
import System.Cached.JSON

import Stackage.MajorVer

latestMajorSnapshot :: MajorVer -> IO (Maybe String)
latestMajorSnapshot ver = do
  lookupKey (T.pack (showMajor ver)) <$> getSnapshots

getSnapshots :: IO Object
getSnapshots =
  getCachedJSON "stackage-snapshots" "snapshots.json" "https://www.stackage.org/download/snapshots.json" 200

latestLTS :: IO MajorVer
latestLTS = do
  msnap <- lookupKey "lts" <$> getSnapshots
  case msnap of
    Just snap -> return $ snapMajorVer snap
    Nothing -> error' "could not resolve lts major version"

latestLtsSnapshot :: IO String
latestLtsSnapshot = do
  msnap <- resolveMajor LatestLTS >>= latestMajorSnapshot
  case msnap of
    Nothing ->
      error' "failed to determine latest lts snapshot"
    Just snap -> return snap

-- converts 'lts' to actual version
resolveMajor :: MajorVerAlias -> IO MajorVer
resolveMajor LatestLTS = latestLTS
resolveMajor (MajorVer ver) = return ver
