module Main where

import Options.Applicative
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Data.Maybe (fromMaybe)
import Unused.Parser (parseLines)
import Unused.Types (ParseResponse, RemovalLikelihood(..))
import Unused.ResponseFilter (withOneOccurrence, withLikelihoods, ignoringPaths)
import Unused.Grouping (CurrentGrouping(..), groupedResponses)
import Unused.CLI (SearchRunner(..), withoutCursor, renderHeader, executeSearch, printParseError, printSearchResults, resetScreen, withInterruptHandler)
import Unused.Cache

data Options = Options
    { oSearchRunner :: SearchRunner
    , oSingleOccurrenceMatches :: Bool
    , oLikelihoods :: [RemovalLikelihood]
    , oAllLikelihoods :: Bool
    , oIgnoredPaths :: [String]
    , oGrouping :: CurrentGrouping
    , oWithCache :: Bool
    }

main :: IO ()
main = withInterruptHandler $
    run =<< execParser
        (withInfo parseOptions pHeader pDescription pFooter)
  where
    pHeader      = "Unused: Analyze potentially unused code"
    pDescription = "Unused allows a developer to pipe in a list of tokens to\
                  \ search through in directory to determine likelihood a\
                  \ token can be removed. Requires tokens be piped into the\
                  \ program seperated by newlines. See CLI USAGE below."
    pFooter      = "CLI USAGE: $ cat path/to/ctags | cut -f1 | sort -u | unused"

run :: Options -> IO ()
run options = withoutCursor $ do
    hSetBuffering stdout NoBuffering

    terms <- pure . lines =<< getContents
    renderHeader terms

    results <- withCache options $ unlines <$> executeSearch (oSearchRunner options) terms
    let response = parseLines results

    resetScreen

    either printParseError (printSearchResults . groupedResponses (oGrouping options)) $
        optionFilters options response

    return ()

withCache :: Options -> IO String -> IO String
withCache Options{ oWithCache = True } = cached
withCache Options{ oWithCache = False } = id

withInfo :: Parser a -> String -> String -> String -> ParserInfo a
withInfo opts h d f =
    info (helper <*> opts) $ header h <> progDesc d <> footer f

optionFilters :: Options -> (ParseResponse -> ParseResponse)
optionFilters o =
    foldl1 (.) filters
  where
    filters =
        [ if oSingleOccurrenceMatches o then withOneOccurrence else id
        , withLikelihoods likelihoods
        , ignoringPaths $ oIgnoredPaths o
        ]
    likelihoods
        | oAllLikelihoods o = [High, Medium, Low]
        | null (oLikelihoods o) = [High]
        | otherwise = oLikelihoods o

parseOptions :: Parser Options
parseOptions =
    Options
    <$> parseSearchRunner
    <*> parseDisplaySingleOccurrenceMatches
    <*> parseLikelihoods
    <*> parseAllLikelihoods
    <*> parseIgnorePaths
    <*> parseGroupings
    <*> parseWithCache

parseSearchRunner :: Parser SearchRunner
parseSearchRunner =
    flag SearchWithProgress SearchWithoutProgress $
        short 'P'
        <> long "no-progress"
        <> help "Don't display progress during analysis"

parseDisplaySingleOccurrenceMatches :: Parser Bool
parseDisplaySingleOccurrenceMatches = switch $
    short 's'
    <> long "single-occurrence"
    <> help "Display only single occurrences"

parseLikelihoods :: Parser [RemovalLikelihood]
parseLikelihoods = many (parseLikelihood <$> parseLikelihoodOption)

parseLikelihood :: String -> RemovalLikelihood
parseLikelihood "high" = High
parseLikelihood "medium" = Medium
parseLikelihood "low" = Low
parseLikelihood _ = Unknown

parseLikelihoodOption :: Parser String
parseLikelihoodOption = strOption $
    short 'l'
    <> long "likelihood"
    <> help "[Allows multiple] [Allowed: high, medium, low] Display results based on likelihood"

parseAllLikelihoods :: Parser Bool
parseAllLikelihoods = switch $
    short 'a'
    <> long "all-likelihoods"
    <> help "Display all likelihoods"

parseIgnorePaths :: Parser [String]
parseIgnorePaths = many $ strOption $
    long "ignore"
    <> metavar "PATH"
    <> help "[Allows multiple] Ignore paths that contain PATH"

parseGroupings :: Parser CurrentGrouping
parseGroupings =
    fromMaybe GroupByDirectory <$> maybeGroup
  where
    maybeGroup = optional $ parseGrouping <$> parseGroupingOption

parseGrouping :: String -> CurrentGrouping
parseGrouping "directory" = GroupByDirectory
parseGrouping "term" = GroupByTerm
parseGrouping "file" = GroupByFile
parseGrouping "none" = NoGroup
parseGrouping _ = NoGroup

parseGroupingOption :: Parser String
parseGroupingOption = strOption $
    short 'g'
    <> long "group-by"
    <> help "[Allowed: directory, term, file, none] Group results"

parseWithCache :: Parser Bool
parseWithCache = switch $
    short 'c'
    <> long "with-cache"
    <> help "Write to cache and read when available"
