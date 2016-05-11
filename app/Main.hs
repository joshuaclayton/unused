module Main where

import Options.Applicative
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Unused.Parser (parseLines)
import Unused.Types (ParseResponse, RemovalLikelihood(..))
import Unused.ResponseFilter (withOneOccurrence, withOneFile, withLikelihoods, ignoringPaths)
import Unused.CLI (SearchRunner(..), executeSearch, printParseError, printSearchResults, resetScreen, withInterruptHandler)

data Options = Options
    { oSearchRunner :: SearchRunner
    , oAllOccurrencesAndFiles :: Bool
    , oLikelihoods :: [RemovalLikelihood]
    , oIgnoredPaths :: [String]
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
run options = do
    hSetBuffering stdout NoBuffering

    terms <- pure . lines =<< getContents
    results <- executeSearch (oSearchRunner options) terms
    let response = parseLines $ unlines results

    resetScreen

    either printParseError printSearchResults $
        optionFilters options response

    return ()

withInfo :: Parser a -> String -> String -> String -> ParserInfo a
withInfo opts h d f =
    info (helper <*> opts) $ header h <> progDesc d <> footer f

optionFilters :: Options -> (ParseResponse -> ParseResponse)
optionFilters o =
    foldl1 (.) filters
  where
    filters =
        [ if oAllOccurrencesAndFiles o then id else withOneOccurrence . withOneFile
        , withLikelihoods $ oLikelihoods o
        , ignoringPaths $ oIgnoredPaths o
        ]

parseOptions :: Parser Options
parseOptions =
    Options
    <$> parseSearchRunner
    <*> parseDisplayAllMatches
    <*> parseLikelihoods
    <*> parseIgnorePaths

parseSearchRunner :: Parser SearchRunner
parseSearchRunner =
    flag SearchWithProgress SearchWithoutProgress $
        short 'P'
        <> long "no-progress"
        <> help "Don't display progress during analysis"

parseDisplayAllMatches :: Parser Bool
parseDisplayAllMatches = switch $
    short 'a'
    <> long "all"
    <> help "Display all files and occurrences"

parseLikelihoods :: Parser [RemovalLikelihood]
parseLikelihoods = many $
    parseLikelihood <$> parseLikelihoodOption

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

parseIgnorePaths :: Parser [String]
parseIgnorePaths = many $ strOption $
    long "ignore"
    <> metavar "PATH"
    <> help "[Allows multiple] Ignore paths that contain PATH"
