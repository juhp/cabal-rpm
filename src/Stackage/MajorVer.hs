module Stackage.MajorVer (
  MajorVer(..),
  readCompactMajor,
  readMajor,
  showMajor,
  showCompact,
  snapMajorVer,
  MajorVerAlias(..),
  eitherReadMajorAlias
  )
where

import Control.Applicative
import Data.List.Extra
import SimpleCmd (error')
import Text.Read (readMaybe)

-- FIXME allow specific snapshots?
data MajorVer = LTS Int | LTSLatest | Nightly
  deriving (Eq, Ord)

maybeReadMajor :: String -> Maybe MajorVer
maybeReadMajor "nightly" = Just Nightly
maybeReadMajor ver =
  if "lts" `isPrefixOf` ver then
    case readMaybe (dropPrefix "lts-" ver) <|> readMaybe (dropPrefix "lts" ver) of
      Just major -> Just (LTS major)
      Nothing -> Nothing
  else Nothing

-- readMajor "lts-16"
readMajor :: String -> MajorVer
readMajor "nightly" = Nightly
readMajor "lts" = LTSLatest
readMajor ver =
  case maybeReadMajor ver of
    Just s -> s
    Nothing ->
      error' $! "couldn't parse " ++ ver ++ " (expected lts-XX or ltsXX)"

-- readCompactMajor "lts16"
-- Should we support "stack-lts.yaml"?
readCompactMajor :: String -> Maybe MajorVer
readCompactMajor "nightly" = Just Nightly
readCompactMajor ver =
  if "lts" `isPrefixOf` ver then
    case readMaybe (dropPrefix "lts" ver) of
      Just major -> Just (LTS major)
      Nothing -> error' $! "couldn't parse compact " ++ ver ++  " (expected ltsXX)"
  else Nothing

showMajor :: MajorVer -> String
showMajor Nightly = "nightly"
showMajor LTSLatest = "lts"
showMajor (LTS ver) = "lts-" ++ show ver

showCompact :: MajorVer -> String
showCompact = filter (/= '-') . showMajor

snapMajorVer :: String -> MajorVer
snapMajorVer snap =
  case breakOn "." snap of
    (major,_suf) -> readMajor major

---- MajorVerAlias

data MajorVerAlias = LatestLTS | MajorVer MajorVer

maybeReadMajorAlias ::  String -> Maybe MajorVerAlias
maybeReadMajorAlias "lts" = Just LatestLTS
maybeReadMajorAlias v = MajorVer <$> maybeReadMajor v

eitherReadMajorAlias :: String -> Either String MajorVerAlias
eitherReadMajorAlias cs =
  case maybeReadMajorAlias cs of
    Just s -> Right s
    _ -> Left cs
