module Unused.Parser
    ( parseLines
    ) where

import Control.Monad (void)
import Data.List (nub)
import Data.Map.Strict (fromList)
import Text.Parsec.String (Parser)
import Text.Parsec
import Unused.Types

parseLines :: String -> ParseResponse
parseLines s =
    either InvalidParse (ValidParse . fromList . groupBy term) $
        parse parseTermMatches "matches" s

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
termChars = alphaNum <|> char '_' <|> char '!' <|> char '?' <|> char '='

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

groupBy :: Eq b => (a -> b) -> [a] -> [(b, [a])]
groupBy f l =
    fmap (\t -> (t, byTerm t)) uniqueTerms
  where
    byTerm t = filter (((==) t) . f) l
    uniqueTerms = nub $ fmap f l
