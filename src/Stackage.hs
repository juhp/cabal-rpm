{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Stackage
-- Copyright   :  (C) 2017-2019  Jens Petersen
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
defaultLTS = LTS "13"

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
    resp <- httpNoBody req mgr
    return $ (fmap B.unpack . lookup "Location" . responseHeaders) resp
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
    Nothing -> return ()
    Just pkgid -> putStrLn $ display pkgid +-+ "in Stackage" +-+ showStream stream
  return mpkgid
