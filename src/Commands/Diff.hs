{-# LANGUAGE CPP #-}

-- |
-- Module      :  Commands.Diff
-- Copyright   :  (C) 2014,2017-2018  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: diff current spec file with cabal-rpm spec output

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Commands.Diff (
  diff
  ) where

import Commands.Spec (createSpecFile)
import PackageUtils (dropChangelog, editSpecField, getSpecField,
                     PackageData (..), prepare)
import Types

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad
import Distribution.Verbosity (silent)
import SimpleCmd (grep_, error', pipe)
import System.FilePath ((<.>))
import System.IO.Extra (withTempDir)
import System.Posix.Env (getEnvDefault)

diff :: Flags -> PackageType -> Maybe PackageVersionSpecifier -> IO ()
diff flags pkgtype mpvs = do
  pkgdata <- prepare flags mpvs
  case specFilename pkgdata of
    Nothing -> error' "No (unique) .spec file in directory."
    Just spec -> do
      subpkg <- grep_ "%{subpkgs}" spec
      withTempDir $ \tmpdir -> do
        speccblrpm <- createSpecFile False silent flags False False pkgtype (if subpkg then Just Nothing else Nothing) Nothing (Just tmpdir) mpvs
        currel <- getSpecField "Release" spec
        let suffix = "%{?dist}"
        editSpecField "Release" (currel ++ suffix) speccblrpm
        diffcmd <- words <$> getEnvDefault "CBLRPM_DIFF" "diff -u"
        out <- dropChangelog <$> pipe (head diffcmd, tail diffcmd ++ [spec, speccblrpm])
        ---- was for %autorelease:
        -- out <- pipe (head diffcmd, tail diffcmd ++ [spec, speccblrpm])
          ("sed", ["-e", "s%" ++ speccblrpm ++ "%" ++ spec <.> "cblrpm" ++ "%"])
        unless (null out) $
          putStrLn out
