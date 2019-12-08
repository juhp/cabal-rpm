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
  latestStackage
  ) where


#ifdef HTTPS
import qualified Data.ByteString.Char8 as B
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.FilePath (takeFileName)
import SimpleCmd ((+-+), removePrefix)
#else
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif
import SimpleCmd ((+-+), cmdMaybe)
import SysCmd (optionalProgram)
#endif
import Distribution.Text (display)
import SimpleCabal (PackageIdentifier(..), PackageName)
import Types

stackageList :: Stream -> PackageName -> IO (Maybe PackageIdentifier)
stackageList stream pkg = do
#ifdef HTTPS
  mgr <- newManager tlsManagerSettings
  let pkgurl = topurl ++ (show stream) ++ "/package/"
  req <- parseRequest $ pkgurl ++ display pkg
  hist <- responseOpenHistory req mgr
  let redirs = mapMaybe (lookup "Location" . responseHeaders . snd) $ hrRedirects hist
  if null redirs
    then return Nothing
    else do
    let loc = B.unpack $ last redirs
    if topurl `isPrefixOf` loc
      then
      let ver = (readVersion . removePrefix (display pkg ++ "-") . takeFileName) loc
      in return $ Just (PackageIdentifier pkg ver)
      else return Nothing
  where
    topurl :: String
    topurl = "https://www.stackage.org/"
#else
  -- check for stackage-query
  haveStackage <- optionalProgram "stackage"
  if haveStackage
    then do
    mver <- cmdMaybe "stackage" ["package", show stream, display pkg]
    return $ PackageIdentifier pkg . readVersion <$> mver
    else do
    putStrLn "'cabal install stackage-query' to check against Stackage LTS"
    return Nothing
#endif

latestStackage :: Stream -> PackageName -> IO (Maybe PackageIdentifier)
latestStackage stream pkg = do
  mpkgid <- stackageList stream pkg
  case mpkgid of
    Nothing -> return ()
    Just pkgid -> putStrLn $ display pkgid +-+ "in Stackage" +-+ show stream
  return mpkgid
