{-# LANGUAGE OverloadedStrings #-}

module Unused.Aliases
    ( groupedTermsAndAliases
    , termsAndAliases
    ) where

import Data.Tuple (swap)
import Data.List (nub, sort, find, (\\))
import Data.Text (Text)
import qualified Data.Text as T
import Unused.ResultsClassifier.Types
import Unused.Types (TermMatch, tmTerm)
import Unused.Util (groupBy)

type Alias = (Text, Text)
type GroupedResult = (String, [TermMatch])

groupedTermsAndAliases :: [TermAlias] -> [TermMatch] -> [[TermMatch]]
groupedTermsAndAliases as ms =
    map snd $ foldl (processResultsWithAliases aliases) [] matchesGroupedByTerm
  where
    matchesGroupedByTerm = groupBy tmTerm ms
    aliases = map toAlias as

termsAndAliases :: [TermAlias] -> [String] -> [String]
termsAndAliases [] = id
termsAndAliases as =
    nub . map T.unpack . concatMap (allAliases aliases . T.pack)
  where
    aliases = map toAlias as
    allAliases :: [Alias] -> Text -> [Text]
    allAliases as' term = concatMap (`generateAliases` term) as'

processResultsWithAliases :: [Alias] -> [GroupedResult] -> GroupedResult -> [GroupedResult]
processResultsWithAliases as acc result@(term, matches) =
    if noAliasesExist
        then acc ++ [result]
        else case closestAlias of
            Nothing -> acc ++ [result]
            Just alias@(aliasTerm, aliasMatches) -> (acc \\ [alias]) ++ [(aliasTerm, aliasMatches ++ matches)]
  where
    packedTerm = T.pack term
    noAliasesExist = null listOfAliases
    listOfAliases = nub (concatMap (`aliasesForTerm` packedTerm) as) \\ [packedTerm]
    closestAlias = find ((`elem` listOfAliases) . T.pack . fst) acc

toAlias :: TermAlias -> Alias
toAlias TermAlias{taFrom = from, taTo = to} = (T.pack from, T.pack to)

generateAliases :: Alias -> Text -> [Text]
generateAliases (from, to) term =
    toTermWithAlias $ parsePatternForMatch from term
  where
    toTermWithAlias (Right (Just match)) = [term, T.replace wildcard match to]
    toTermWithAlias _ = [term]

parsePatternForMatch :: Text -> Text -> Either Text (Maybe Text)
parsePatternForMatch aliasPattern term =
    findMatch $ T.splitOn wildcard aliasPattern
  where
    findMatch [prefix, suffix] = Right $ T.stripSuffix suffix =<< T.stripPrefix prefix term
    findMatch _ = Left $ T.pack $ "There was a problem with the pattern: " ++ show aliasPattern

aliasesForTerm :: Alias -> Text -> [Text]
aliasesForTerm a t = nub $ sort $ generateAliases a t ++ generateAliases (swap a) t

wildcard :: Text
wildcard = "%s"
