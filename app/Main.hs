module Main where

import App
import Options.Applicative
import Data.Maybe (fromMaybe)
import Unused.Grouping (CurrentGrouping(..))
import Unused.Types (RemovalLikelihood(..))
import Unused.CLI (SearchRunner(..))
import Unused.Util (stringToInt)

main :: IO ()
main = runProgram =<< parseCLI

parseCLI :: IO Options
parseCLI =
    execParser (withInfo parseOptions pHeader pDescription pFooter)
  where
    pHeader      = "Unused: Analyze potentially unused code"
    pDescription = "Unused allows a developer to leverage an existing tags file\
                  \ (located at .git/tags, tags, or tmp/tags) to identify tokens\
                  \ in a codebase that are unused."
    pFooter      = "CLI USAGE: $ unused"

withInfo :: Parser a -> String -> String -> String -> ParserInfo a
withInfo opts h d f =
    info (helper <*> opts) $ header h <> progDesc d <> footer f

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
    <*> parseCommitCount

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

parseCommitCount :: Parser (Maybe Int)
parseCommitCount =
    (stringToInt =<<) <$> commitParser
  where
    commitParser = optional $ strOption $
        long "commits"
        <> help "Number of recent commit SHAs to display per token"
