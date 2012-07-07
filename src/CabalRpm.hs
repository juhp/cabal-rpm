-- |
-- Module      :  CabalRpm
-- Copyright   :  Bryan O'Sullivan 2007
--
-- Maintainer  :  Bryan O'Sullivan <bos@serpentine.com>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Main entry point for building RPM packages.

-- This software may be used and distributed according to the terms of
-- the GNU General Public License, incorporated herein by reference.

module Main where

import qualified Distribution.Package.Rpm.Main as Rpm

main :: IO ()

main = Rpm.main
