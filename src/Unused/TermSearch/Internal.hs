{-# LANGUAGE OverloadedStrings #-}

module Unused.TermSearch.Internal
    ( commandLineOptions
    , parseSearchResult
    ) where

import qualified Data.Char as C
import qualified Data.Maybe as M
import qualified Data.Text as T
import           Unused.Types (SearchTerm(..), TermMatch(..))
import           Unused.Util (stringToInt)

commandLineOptions :: String -> [String]
commandLineOptions t =
    if regexSafeTerm t
        then ["(\\W|^)" ++ t ++ "(\\W|$)", "."] ++ baseFlags
        else [t, ".", "-Q"] ++ baseFlags
  where
    baseFlags = ["-c", "--ackmate", "--ignore-dir", "tmp/unused"]

parseSearchResult :: SearchTerm -> String -> Maybe TermMatch
parseSearchResult term =
    maybeTermMatch . map T.unpack . T.splitOn ":" . T.pack
  where
    maybeTermMatch [_, path, count] = Just $ toTermMatch term path $ countInt count
    maybeTermMatch _ = Nothing
    countInt = M.fromMaybe 0 . stringToInt
    toTermMatch (OriginalTerm t) path = TermMatch t path Nothing
    toTermMatch (AliasTerm t a) path = TermMatch t path (Just a)

regexSafeTerm :: String -> Bool
regexSafeTerm = all (\c -> C.isAlphaNum c || c == '_' || c == '-')
