{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Stackage
-- Copyright   :  (C) 2017-2021  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: queries Stackage package versions

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Stackage (
  defaultLTS,
  latestStackage
  ) where


#ifdef CURL
import Network.Curl
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
#else
import qualified Data.ByteString.Char8 as B
import Network.HTTP.Client
import Network.HTTP.Client.TLS
#endif
import Data.Maybe (fromMaybe)
import Distribution.Text (display)
import SimpleCabal (PackageIdentifier(..), PackageName)
import SimpleCmd ((+-+), cmdFull, removePrefix)
import System.FilePath (takeFileName)

import qualified Stackage.MajorVer as MV
import Stackage.Snapshots (latestMajorSnapshot, latestLTS)
import SysCmd (optionalProgram)
import Types

defaultLTS :: Stream
defaultLTS = LTS 22

streamToMajorVer :: Stream -> MV.MajorVer
streamToMajorVer (LTS n) = MV.LTS n
streamToMajorVer LatestLTS = MV.LTSLatest
streamToMajorVer LatestNightly = MV.Nightly
streamToMajorVer _ = error "unsupported/impossible stream"

majorVerToStream :: MV.MajorVer -> Stream
majorVerToStream (MV.LTS n) = LTS n
majorVerToStream MV.LTSLatest = LatestLTS
majorVerToStream MV.Nightly = LatestNightly

stackageList :: Stream -> PackageName -> IO (Maybe PackageIdentifier)
stackageList stream pkg = do
  haveStack <- optionalProgram "stack"
  if haveStack
    then do
    msnap <- latestMajorSnapshot $ streamToMajorVer stream
    case msnap of
      Nothing -> error "snapshot not found"
      Just snap -> do
        (ok,out,err) <- cmdFull "stack" ["--resolver", snap, "--verbosity", "info", "list", display pkg] ""
        if not ok
          then return Nothing
          else do
          let pkgver = if null out then err else out
              ver = (readVersion . removePrefix (display pkg ++ "-")) pkgver
          return $ Just $ PackageIdentifier pkg ver
    else do
    let pkgurl = "https://www.stackage.org/" ++ showStream stream ++ "/package/" ++ display pkg
    mloc <-
#ifdef CURL
      withCurlDo $
        (lookup "location" . reverse . snd) <$> curlHead pkgurl [CurlFollowLocation True]
#else
      do
      mgr <- newManager tlsManagerSettings
      req <- parseRequest pkgurl
      hist <- responseOpenHistory (req {method = "HEAD"}) mgr
      let redirs = hrRedirects hist
      if null redirs
        then return Nothing
        else return $ (fmap B.unpack . lookup "Location" . responseHeaders . snd . last) redirs
#endif
    case mloc of
      Nothing -> return Nothing
      Just loc -> do
        let file = takeFileName loc
        -- check if no version
        if file == display pkg then
          return Nothing
          else
          let ver = (readVersion . removePrefix (display pkg ++ "-")) file in
          return $ Just $ PackageIdentifier pkg ver

latestStackage :: Maybe Stream -> PackageName -> IO (Maybe PackageIdentifier)
latestStackage mstream pkg = do
  let stream = fromMaybe defaultLTS mstream
  mpkgid <- stackageList stream pkg
  case mpkgid of
    Just pkgid -> do
      putStrLn $ display pkgid +-+ "in Stackage" +-+ showStream stream
      return mpkgid
    Nothing ->
      newerStream stream >>=
      maybe (return Nothing) (\ nstream -> latestStackage (Just nstream) pkg)
  where
    newerStream :: Stream -> IO (Maybe Stream)
    newerStream (LTS n) = do
      latest <- latestLTS
      return $
        if LTS n < majorVerToStream latest
        then Just (LTS (n+1))
        else Just LatestNightly
    newerStream LatestLTS = return $ Just LatestNightly
    newerStream _ = return Nothing
