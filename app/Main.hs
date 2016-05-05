module Main where

import Options.Applicative
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Unused.Parser (parseLines)
import Unused.Types (withOneOccurrence, withOneFile)
import Unused.CLI (SearchRunner(..), executeSearch, printParseError, printSearchResults, resetScreen)

data Options = Options
    { oSearchRunner :: SearchRunner
    }

main :: IO ()
main = run =<< execParser
    (parseOptions `withInfo` "Analyze potentially unused code")

run :: Options -> IO ()
run options = do
    hSetBuffering stdout NoBuffering

    terms <- pure . lines =<< getContents
    results <- executeSearch (oSearchRunner options) terms
    let response = parseLines $ unlines results

    resetScreen

    either printParseError printSearchResults $
        withOneOccurrence $ withOneFile response

    return ()

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseOptions :: Parser Options
parseOptions =
    Options
    <$> parseSearchRunner
        (short 'P' <> long "no-progress" <> help "Don't display progress during analysis")

parseSearchRunner :: Mod FlagFields SearchRunner -> Parser SearchRunner
parseSearchRunner =
    flag SearchWithProgress SearchWithoutProgress
