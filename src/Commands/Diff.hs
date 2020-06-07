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
import FileUtils (mktempdir)
import PackageUtils (PackageData (..), prepare)
import SysCmd (die, optionalProgram)
import Types

import SimpleCmd (grep_, pipe_, pipe3_)

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif
import Distribution.Verbosity (silent)
import System.Directory (removeDirectoryRecursive)
import System.FilePath ((<.>))
import System.Posix.Env (getEnvDefault)

diff :: Flags -> PackageType -> Maybe PackageVersionSpecifier -> IO ()
diff flags pkgtype mpvs = do
  pkgdata <- prepare flags mpvs True
  case specFilename pkgdata of
    Nothing -> die "No (unique) .spec file in directory."
    Just spec -> do
      subpkg <- grep_ "%{subpkgs}" spec
      tmpdir <- mktempdir
      speccblrpm <- createSpecFile silent flags False False pkgtype subpkg (Just tmpdir) mpvs
      diffcmd <- words <$> getEnvDefault "CBLRPM_DIFF" "diff -u"
      hawk <- optionalProgram "hawk"
      if hawk
        then
        pipe3_ (head diffcmd, tail diffcmd ++ [spec, speccblrpm])
          ("sed", ["-e", "s%" ++ speccblrpm ++ "%" ++ spec <.> "cblrpm" ++ "%"])
          ("hawk", ["-d''", "-a", "takeWhile (/= [\"%changelog\"])"])
        else
        pipe_ (head diffcmd, tail diffcmd ++ [spec, speccblrpm])
        ("sed", ["-e", "s%" ++ speccblrpm ++ "%" ++ spec <.> "cblrpm" ++ "%"])
      removeDirectoryRecursive tmpdir
