module Header (
  withSpecHead,
  headerOption,
  headerVersion
              )
where

import Data.List (isPrefixOf)
import Safe (headMay, tailSafe)
import SimpleCmd (removePrefix, (+-+))

import FileUtils (assertFileNonEmpty)

withSpecHead :: FilePath -> ([String] -> IO a) -> IO a
withSpecHead spec act = do
  assertFileNonEmpty spec
  readFile spec >>= (act . headerWords)
  where
    headerWords :: String -> [String]
    headerWords s =
      case headMay $ lines s of
        Nothing -> error $ "empty" +-+ spec
        Just h -> words h

headerVersion :: [String] -> String
headerVersion headerwords =
  let cblrpm = filter ("cabal-rpm-" `isPrefixOf`) headerwords
  in case cblrpm of
       [nv] -> removePrefix "cabal-rpm-" nv
       _ -> "0.9.11"

headerOption :: String -> [String] -> Maybe String
headerOption opt =
  headMay . tailSafe . dropWhile (/= opt)
