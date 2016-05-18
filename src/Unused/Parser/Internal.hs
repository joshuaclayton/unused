module Unused.Parser.Internal
    ( parseTermMatches
    , parseTermMatch
    , parse
    , ParseError
    ) where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Text.Parsec
import Text.Parsec.String (Parser)
import Unused.Types (TermMatch(..))
import Unused.Util (stringToInt)

parseTermMatches :: Parser [TermMatch]
parseTermMatches = many1 parseTermMatch <* eof

parseTermMatch :: Parser TermMatch
parseTermMatch = do
    colonSep
    path <- pathParser
    colonSep
    occurrences <- occurrenceParser
    colonSep
    term <- termParser
    void eol

    return $ TermMatch term path occurrences
  where
    colonSep = void $ try $ char ':'

termParser :: Parser String
termParser = many1 (noneOf "\n")

pathParser :: Parser String
pathParser = many1 (noneOf ":")

occurrenceParser :: Parser Int
occurrenceParser =
    fromMaybe 0 . stringToInt <$> many1 digit

eol :: Parser String
eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"
