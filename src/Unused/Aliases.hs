module Unused.Aliases
    ( groupedTermsAndAliases
    , termsAndAliases
    ) where

import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Unused.ResultsClassifier.Types
import Unused.Types (SearchTerm(..), TermMatch, tmTerm)
import Unused.Util (groupBy)

groupedTermsAndAliases :: [TermMatch] -> [[TermMatch]]
groupedTermsAndAliases = map snd . groupBy tmTerm

termsAndAliases :: [TermAlias] -> [String] -> [SearchTerm]
termsAndAliases [] = map OriginalTerm
termsAndAliases as = L.nub . concatMap ((as >>=) . generateSearchTerms . T.pack)

generateSearchTerms :: Text -> TermAlias -> [SearchTerm]
generateSearchTerms term TermAlias {taFrom = from, taTransform = transform} =
    toTermWithAlias $ parsePatternForMatch (T.pack from) term
  where
    toTermWithAlias (Right (Just match)) =
        [OriginalTerm unpackedTerm, AliasTerm unpackedTerm (aliasedResult match)]
    toTermWithAlias _ = [OriginalTerm unpackedTerm]
    unpackedTerm = T.unpack term
    aliasedResult = T.unpack . transform

parsePatternForMatch :: Text -> Text -> Either Text (Maybe Text)
parsePatternForMatch aliasPattern term = findMatch $ T.splitOn wildcard aliasPattern
  where
    findMatch [prefix, suffix] = Right $ T.stripSuffix suffix =<< T.stripPrefix prefix term
    findMatch _ = Left $ T.pack $ "There was a problem with the pattern: " ++ show aliasPattern

wildcard :: Text
wildcard = "*"
