module Header (
  withSpecHead,
  headerOption,
  headerVersion
              )
where

import Data.List (isPrefixOf)
import SimpleCmd (removePrefix)

withSpecHead :: FilePath -> ([String] -> IO a) -> IO a
withSpecHead spec act =
  readFile spec >>= (act . headerWords)

headerWords :: String -> [String]
headerWords = words . head . lines

headerVersion :: [String] -> String
headerVersion headerwords =
  let cblrpm = filter ("cabal-rpm-" `isPrefixOf`) headerwords
  in case cblrpm of
       [nv] -> removePrefix "cabal-rpm-" nv
       _ -> "0.9.11"

headerOption :: [String] -> String -> Maybe String
headerOption headerwords opt =
  if opt `elem` headerwords
  then (Just . head . tail . dropWhile (/= opt)) headerwords
  else Nothing
