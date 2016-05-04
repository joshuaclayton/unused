module Unused.Parser
    ( parseLines
    , ParseError
    ) where

import Control.Monad (void)
import Data.Bifunctor (second)
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Data.Map.Strict as Map
import Unused.Util (groupBy)
import Unused.Types (TermMatch(..), ParseResponse, resultsFromMatches)

parseLines :: String -> ParseResponse
parseLines =
    responseFromParse . parse parseTermMatches "matches"

responseFromParse :: Either ParseError [TermMatch] -> ParseResponse
responseFromParse =
    fmap $ Map.fromList . map (second resultsFromMatches) . groupBy tmTerm

parseTermMatches :: Parser [TermMatch]
parseTermMatches = do
    tm <- many1 $ do
        m <- parseTermMatch
        void eol

        return m
    eof

    return tm

parseTermMatch :: Parser TermMatch
parseTermMatch = do
    term' <- termParser
    colonSep
    path' <- pathParser
    colonSep
    occurrences' <- occurrenceParser

    return $ TermMatch term' path' $ toInt occurrences'
  where
    toInt i = read i :: Int
    colonSep = do { void $ try $ char ':' }

termChars :: Parser Char
termChars = choice [alphaNum, char '_', char '!', char '?', char '=', char '>', char '<', char '[', char ']', char '.']

termParser :: Parser String
termParser = many1 termChars

pathParser :: Parser String
pathParser = many1 (noneOf ":")

occurrenceParser :: Parser String
occurrenceParser = many1 digit

eol :: Parser String
eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"
