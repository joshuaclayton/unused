{-# LANGUAGE OverloadedStrings #-}

module Unused.TermSearch.Internal
    ( commandLineOptions
    , parseSearchResult
    ) where

import qualified Data.Char as C
import qualified Data.Maybe as M
import qualified Data.Text as T
import           Unused.Types (TermMatch(..))
import           Unused.Util (stringToInt)

commandLineOptions :: String -> [String]
commandLineOptions t =
    if regexSafeTerm t
        then ["(\\W|^)" ++ t ++ "(\\W|$)", "."] ++ baseFlags
        else [t, ".", "-Q"] ++ baseFlags
  where
    baseFlags = ["-c", "--ackmate", "--ignore-dir", "tmp/unused"]

parseSearchResult :: String -> String -> Maybe TermMatch
parseSearchResult term =
    toTermMatch . map T.unpack . T.splitOn ":" . T.pack
  where
    toTermMatch [_, path, count] = Just $ TermMatch term path (countInt count)
    toTermMatch _ = Nothing
    countInt = M.fromMaybe 0 . stringToInt

regexSafeTerm :: String -> Bool
regexSafeTerm = all (\c -> C.isAlphaNum c || c == '_' || c == '-')
