{-# LANGUAGE OverloadedStrings #-}

module Unused.TermSearch.Internal
    ( commandLineOptions
    , parseSearchResult
    ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Unused.Types (TermMatch(..))
import Unused.Regex
import Unused.Util (stringToInt)

commandLineOptions :: String -> [String]
commandLineOptions t =
    case regexSafeTerm t of
        True -> ["(\\W|^)" ++ t ++ "(\\W|$)", "."] ++ baseFlags
        False -> [t, ".", "-Q"] ++ baseFlags
  where
    baseFlags = ["-c", "--ackmate", "--ignore-dir", "tmp/unused"]

parseSearchResult :: String -> String -> Maybe TermMatch
parseSearchResult term s =
    toTermMatch $ map T.unpack $ T.splitOn ":" $ T.pack s
  where
    toTermMatch [_, path, count] = Just $ TermMatch term path (countInt count)
    toTermMatch _ = Nothing
    countInt = fromMaybe 0 . stringToInt

regexSafeTerm :: String -> Bool
regexSafeTerm =
    matchRegex "^[[:word:]]+$"
