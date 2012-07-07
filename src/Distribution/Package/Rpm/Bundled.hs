-- |
-- Module      :  Distribution.Package.Rpm.Bundled
-- Copyright   :  Bryan O'Sullivan 2008
--
-- Maintainer  :  Bryan O'Sullivan <bos@serpentine.com>
-- Stability   :  alpha
-- Portability :  portable
--
-- Determine whether a specific version of a Haskell package is
-- bundled with into this particular version of the given compiler.

-- This software may be used and distributed according to the terms of
-- the GNU General Public License, incorporated herein by reference.

module Distribution.Package.Rpm.Bundled
    (
      bundledWith
    , isBundled
    ) where

import Data.List (find)
import Data.Version (Version(..))
import Distribution.Simple.Compiler (Compiler(..), CompilerFlavor(..))
import Distribution.Package (PackageIdentifier(..))
import Distribution.Version (Dependency(..), withinRange)

-- | List the packages bundled with this version of the given
-- compiler.  If the answer is not known, return the empty list.
bundledWith :: Compiler -> Maybe [PackageIdentifier]
bundledWith c =
    let cv = (compilerFlavor c, pkgVersion $ compilerId c)
    in thd `fmap` find (\(n,v,_) -> (n,v) == cv) builtIns
  where thd (_,_,x) = x

-- | Determine whether a specific version of a Haskell package is
-- bundled with into this particular version of the given compiler.
isBundled :: Compiler -> Dependency -> Bool
isBundled c (Dependency pkg version) = maybe False checkVersion $ do
    let cv = (compilerFlavor c, pkgVersion $ compilerId c)
    (_, _, cb) <- find (\(n, k, _) -> (n,k) == cv) builtIns
    pkgVersion `fmap` find ((== pkg) . pkgName) cb
  where checkVersion = flip withinRange version

builtIns :: [(CompilerFlavor, Version, [PackageIdentifier])]
builtIns = [
    (GHC, Version [6,8,2] [], ghc682BuiltIns)
  , (GHC, Version [6,8,1] [], ghc681BuiltIns)
  , (GHC, Version [6,6,1] [], ghc661BuiltIns)
  , (GHC, Version [6,6] [], ghc66BuiltIns)
  ]

v :: String -> [Int] -> PackageIdentifier
v n x = PackageIdentifier n (Version x [])

ghc682BuiltIns :: [PackageIdentifier]
ghc682BuiltIns = [
    v "Cabal" [1,2,3,0],
    v "GLUT" [2,1,1,1],
    v "HUnit" [1,2,0,0],
    v "OpenAL" [1,3,1,1],
    v "OpenGL" [2,2,1,1],
    v "QuickCheck" [1,1,0,0],
    v "array" [0,1,0,0],
    v "base" [3,0,1,0],
    v "bytestring" [0,9,0,1],
    v "cgi" [3001,1,5,1],
    v "containers" [0,1,0,1],
    v "directory" [1,0,0,0],
    v "fgl" [5,4,1,1],
    v "filepath" [1,1,0,0],
    v "haskell-src" [1,0,1,1],
    v "haskell98" [1,0,1,0],
    v "hpc" [0,5,0,0],
    v "html" [1,0,1,1],
    v "mtl" [1,1,0,0],
    v "network" [2,1,0,0],
    v "old-locale" [1,0,0,0],
    v "old-time" [1,0,0,0],
    v "packedstring" [0,1,0,0],
    v "parallel" [1,0,0,0],
    v "parsec" [2,1,0,0],
    v "pretty" [1,0,0,0],
    v "process" [1,0,0,0],
    v "random" [1,0,0,0],
    v "readline" [1,0,1,0],
    v "regex-base" [0,72,0,1],
    v "regex-compat" [0,71,0,1],
    v "regex-posix" [0,72,0,2],
    v "stm" [2,1,1,0],
    v "template-haskell" [2,2,0,0],
    v "time" [1,1,2,0],
    v "unix" [2,3,0,0],
    v "xhtml" [3000,0,2,1]
    ]

ghc681BuiltIns :: [PackageIdentifier]
ghc681BuiltIns = [
    v "base" [3,0,0,0],
    v "Cabal" [1,2,2,0],
    v "GLUT" [2,1,1,1],
    v "HGL" [3,2,0,0],
    v "HUnit" [1,2,0,0],
    v "OpenAL" [1,3,1,1],
    v "OpenGL" [2,2,1,1],
    v "QuickCheck" [1,1,0,0],
    v "X11" [1,2,3,1],
    v "array" [0,1,0,0],
    v "bytestring" [0,9,0,1],
    v "cgi" [3001,1,5,1],
    v "containers" [0,1,0,0],
    v "directory" [1,0,0,0],
    v "fgl" [5,4,1,1],
    v "filepatch" [1,1,0,0],
    v "haskell-src" [1,0,1,1],
    v "haskell98" [1,0,1,0],
    v "hpc" [0,5,0,0],
    v "html" [1,0,1,1],
    v "mtl" [1,1,0,0],
    v "network" [2,1,0,0],
    v "old-locale" [1,0,0,0],
    v "old-time" [1,0,0,0],
    v "packedstring" [0,1,0,0],
    v "parallel" [1,0,0,0],
    v "parsec" [2,1,0,0],
    v "pretty" [1,0,0,0],
    v "process" [1,0,0,0],
    v "random" [1,0,0,0],
    v "readline" [1,0,1,0],
    v "regex-base" [0,72,0,1],
    v "regex-compat" [0,71,0,1],
    v "regex-posix" [0,72,0,1],
    v "stm" [2,1,1,0],
    v "template-haskell" [2,2,0,0],
    v "time" [1,1,2,0],
    v "unix" [2,2,0,0],
    v "xhtml" [3000,0,2,1]
    ]

ghc661BuiltIns :: [PackageIdentifier]
ghc661BuiltIns = [
    v "base" [2,1,1],
    v "Cabal" [1,1,6,2],
    v "cgi" [3001,1,1],
    v "fgl" [5,4,1],
    v "filepath" [1,0],
    v "ghc" [6,6,1],
    v "GLUT" [2,1,1],
    v "haskell98" [1,0],
    v "haskell-src" [1,0,1],
    v "HGL" [3,1,1],
    v "html" [1,0,1],
    v "HUnit" [1,1,1],
    v "mtl" [1,0,1],
    v "network" [2,0,1],
    v "OpenAL" [1,3,1],
    v "OpenGL" [2,2,1],
    v "parsec" [2,0],
    v "QuickCheck" [1,0,1],
    v "readline" [1,0],
    v "regex-base" [0,72],
    v "regex-compat" [0,71],
    v "regex-posix" [0,71],
    v "rts" [1,0],
    v "stm" [2,0],
    v "template-haskell" [2,1],
    v "time" [1,1,1],
    v "unix" [2,1],
    v "X11" [1,2,1],
    v "xhtml" [3000,0,2]
    ]

ghc66BuiltIns :: [PackageIdentifier]
ghc66BuiltIns = [
    v "base" [2,0],
    v "Cabal" [1,1,6],
    v "cgi" [2006,9,6],
    v "fgl" [5,2],
    v "ghc" [6,6],
    v "GLUT" [2,0],
    v "haskell98" [1,0],
    v "haskell-src" [1,0],
    v "HGL" [3,1],
    v "html" [1,0],
    v "HTTP" [2006,7,7],
    v "HUnit" [1,1],
    v "mtl" [1,0],
    v "network" [2,0],
    v "OpenAL" [1,3],
    v "OpenGL" [2,1],
    v "parsec" [2,0],
    v "QuickCheck" [1,0],
    v "readline" [1,0],
    v "regex-base" [0,71],
    v "regex-compat" [0,71],
    v "regex-posix" [0,71],
    v "rts" [1,0],
    v "stm" [2,0],
    v "template-haskell" [2,0],
    v "time" [1,0],
    v "unix" [1,0],
    v "X11" [1,1],
    v "xhtml" [2006,9,13]
    ]
