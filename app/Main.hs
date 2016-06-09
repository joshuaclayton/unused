module Main where

import Options.Applicative
import Data.Maybe (fromMaybe)
import Unused.Parser (parseResults)
import Unused.Types (TermMatchSet, RemovalLikelihood(..))
import Unused.TermSearch (SearchResults(..), fromResults)
import Unused.ResultsClassifier
import Unused.ResponseFilter (withOneOccurrence, withLikelihoods, ignoringPaths)
import Unused.Grouping (CurrentGrouping(..), groupedResponses)
import Unused.CLI (SearchRunner(..), withRuntime, renderHeader, executeSearch)
import qualified Unused.CLI.Views as V
import Unused.Cache
import Unused.Aliases (termsAndAliases)
import Unused.TagsSource

data Options = Options
    { oSearchRunner :: SearchRunner
    , oSingleOccurrenceMatches :: Bool
    , oLikelihoods :: [RemovalLikelihood]
    , oAllLikelihoods :: Bool
    , oIgnoredPaths :: [String]
    , oGrouping :: CurrentGrouping
    , oWithoutCache :: Bool
    , oFromStdIn :: Bool
    }

main :: IO ()
main =
    run =<< execParser
        (withInfo parseOptions pHeader pDescription pFooter)
  where
    pHeader      = "Unused: Analyze potentially unused code"
    pDescription = "Unused allows a developer to leverage an existing tags file\
                  \ (located at .git/tags, tags, or tmp/tags) to identify tokens\
                  \ in a codebase that are unused."
    pFooter      = "CLI USAGE: $ unused"

run :: Options -> IO ()
run options = withRuntime $ do
    terms' <- calculateTagInput options

    case terms' of
       (Left e) -> V.missingTagsFileError e
       (Right terms'') -> do
            languageConfig <- loadLanguageConfig

            let terms = termsWithAlternatesFromConfig languageConfig terms''

            renderHeader terms
            results <- withCache options $ executeSearch (oSearchRunner options) terms

            printResults options $ parseResults languageConfig results

    return ()

termsWithAlternatesFromConfig :: [LanguageConfiguration] -> [String] -> [String]
termsWithAlternatesFromConfig lcs =
    termsAndAliases aliases
  where
    aliases = concatMap lcTermAliases lcs

printResults :: Options -> TermMatchSet -> IO ()
printResults options = V.searchResults . groupedResponses (oGrouping options) . optionFilters options

loadLanguageConfig :: IO [LanguageConfiguration]
loadLanguageConfig = either (const []) id <$> loadConfig

calculateTagInput :: Options -> IO (Either TagSearchOutcome [String])
calculateTagInput Options{ oFromStdIn = True } = loadTagsFromPipe
calculateTagInput Options{ oFromStdIn = False } = loadTagsFromFile

withCache :: Options -> IO SearchResults -> IO SearchResults
withCache Options{ oWithoutCache = True } = id
withCache Options{ oWithoutCache = False } = fmap SearchResults . cached "term-matches" . fmap fromResults

withInfo :: Parser a -> String -> String -> String -> ParserInfo a
withInfo opts h d f =
    info (helper <*> opts) $ header h <> progDesc d <> footer f

optionFilters :: Options -> (TermMatchSet -> TermMatchSet)
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
    <*> parseWithoutCache
    <*> parseFromStdIn

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

parseWithoutCache :: Parser Bool
parseWithoutCache = switch $
    short 'C'
    <> long "no-cache"
    <> help "Ignore cache when performing calculations"

parseFromStdIn :: Parser Bool
parseFromStdIn = switch $
    long "stdin"
    <> help "Read tags from STDIN"
