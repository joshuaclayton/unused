module Unused.TermSearch.Internal
    ( commandLineOptions
    , parseSearchResult
    ) where

import qualified Data.Char as C
import qualified Data.Maybe as M
import qualified Data.Text as T
import Unused.TermSearch.Types (SearchBackend(..))
import Unused.Types (SearchTerm(..), TermMatch(..))
import Unused.Util (stringToInt)

commandLineOptions :: SearchBackend -> String -> [String]
commandLineOptions backend t =
    if regexSafeTerm t
        then regexFlags backend t ++ baseFlags backend
        else nonRegexFlags backend t ++ baseFlags backend

parseSearchResult :: SearchBackend -> SearchTerm -> String -> Maybe TermMatch
parseSearchResult backend term = maybeTermMatch backend . map T.unpack . T.splitOn ":" . T.pack
  where
    maybeTermMatch Rg [path, count] = Just $ toTermMatch term path $ countInt count
    maybeTermMatch Rg _ = Nothing
    maybeTermMatch Ag [_, path, count] = Just $ toTermMatch term path $ countInt count
    maybeTermMatch Ag _ = Nothing
    countInt = M.fromMaybe 0 . stringToInt
    toTermMatch (OriginalTerm t) path = TermMatch t path Nothing
    toTermMatch (AliasTerm t a) path = TermMatch t path (Just a)

regexSafeTerm :: String -> Bool
regexSafeTerm = all (\c -> C.isAlphaNum c || c == '_' || c == '-')

nonRegexFlags :: SearchBackend -> String -> [String]
nonRegexFlags Rg t = [t, ".", "-F"]
nonRegexFlags Ag t = [t, ".", "-Q"]

baseFlags :: SearchBackend -> [String]
baseFlags Rg = ["-c", "-j", "1"]
baseFlags Ag = ["-c", "--ackmate", "--ignore-dir", "tmp/unused"]

regexFlags :: SearchBackend -> String -> [String]
regexFlags _ t = ["(\\W|^)" ++ t ++ "(\\W|$)", "."]
