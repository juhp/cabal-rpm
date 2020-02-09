{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Stackage
-- Copyright   :  (C) 2017-2020  Jens Petersen
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
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
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
import SimpleCmd ((+-+), removePrefix)
import System.FilePath (takeFileName)
import Types

defaultLTS :: Stream
defaultLTS = LTS "14"

latestLTS :: Stream
latestLTS = LTS "14"

stackageList :: Stream -> PackageName -> IO (Maybe PackageIdentifier)
stackageList stream pkg = do
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
    Nothing -> case mstream of
      Nothing -> if defaultLTS == latestLTS then return Nothing
                 else latestStackage (Just LatestLTS) pkg
      Just s -> case s of
        LTS n | LTS n < latestLTS -> latestStackage (Just LatestLTS) pkg
        LTS n | LTS n >= latestLTS -> latestStackage (Just LatestNightly) pkg
        LatestLTS -> latestStackage (Just LatestNightly) pkg
        _ -> return Nothing
