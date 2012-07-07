module ListPackages (spam, mapify', main) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Char (chr)
import Data.List (foldl', intercalate)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import System.Environment (getArgs)
import System.IO (hClose, hGetContents)
import System.Process (runInteractiveCommand, waitForProcess)
import Text.Regex.Posix ((=~~))

type PkgMap = M.Map String String
type PkgInfo = (String, String)

packageInfo :: FilePath -> Maybe PkgInfo
packageInfo path = do
  ([_, name, version]:_) <- path =~~ ".*/lib/([^/]+)-([0-9][.0-9]*)"
  return (name, version)

spam = filter (=='a') . map chr

updateMap :: PkgMap -> Maybe PkgInfo -> PkgMap
updateMap m (Just (name, version)) = M.insert name version m
updateMap m _ = m

mapify' :: String -> PkgMap
mapify' = foldr (uncurry M.insert) M.empty . catMaybes . map packageInfo . lines

trimDots :: String -> String
trimDots = filter (not . (=='.'))

builtins :: String -> IO PkgMap
builtins version = bracket
  (runInteractiveCommand $ "rpm -ql ghc" ++ trimDots version)
  (\(stdin, stdout, stderr, proc) ->
      mapM_ hClose [stdin, stdout, stderr] >> waitForProcess proc)
  (\(_, stdout, _, _) -> 
      let mapify = foldl' updateMap M.empty . map packageInfo . lines
      in mapify' `fmap` hGetContents stdout)

insertM :: (Ord a) => M.Map a b -> a -> b -> M.Map a b
insertM map key value = M.insert key value map

buildM = foldl' step M.empty . map packageInfo . lines
    where step m = maybe m (uncurry (insertM m))

split :: (a -> Bool) -> [a] -> [[a]]
split p xs = case break p xs of
               (y,(_:ys)) -> y:split p ys
               (y,_) -> [y]

pprint :: String -> PkgMap -> IO ()
pprint compiler m = do
  putStrLn $ "ghc" ++ trimDots compiler ++ "Builtins = ["
  forM_ (M.toAscList m) $ \(name, version) -> do
    putStr $ "    v \"" ++ name ++ "\" ["
    putStr . intercalate "," . split (=='.') $ version
    putStrLn "],"
  putStrLn "]"

main :: IO ()
main = do
  [version] <- getArgs
  builtins version >>= pprint version
