module Header (
  withSpecHead,
  headerOption,
  headerVersion
              )
where

import Data.List (isPrefixOf)
import SimpleCmd (removePrefix)

import FileUtils (assertFileNonEmpty)

withSpecHead :: FilePath -> ([String] -> IO a) -> IO a
withSpecHead spec act = do
  assertFileNonEmpty spec
  readFile spec >>= (act . headerWords)
  where
    headerWords :: String -> [String]
    headerWords = words . head . lines

headerVersion :: [String] -> String
headerVersion headerwords =
  let cblrpm = filter ("cabal-rpm-" `isPrefixOf`) headerwords
  in case cblrpm of
       [nv] -> removePrefix "cabal-rpm-" nv
       _ -> "0.9.11"

headerOption :: String -> [String] -> Maybe String
headerOption opt headerwords =
  if opt `elem` headerwords
  then (Just . head . tail . dropWhile (/= opt)) headerwords
  else Nothing
