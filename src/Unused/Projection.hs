{-# LANGUAGE OverloadedStrings #-}

module Unused.Projection where

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec
import           Text.Megaparsec.Text
import           Unused.Projection.Transform

data ParsedTransform = ParsedTransform
    { ptPre :: Text
    , ptTransforms :: [Transform]
    , ptPost :: Text
    }

translate :: Text -> Either ParseError (Text -> Text)
translate template = applyTransform <$> parseTransform template

applyTransform :: ParsedTransform -> Text -> Text
applyTransform pt t =
    ptPre pt
    <> runTransformations t (ptTransforms pt)
    <> ptPost pt

parseTransform :: Text -> Either ParseError ParsedTransform
parseTransform = parse parsedTransformParser ""

parsedTransformParser :: Parser ParsedTransform
parsedTransformParser =
    ParsedTransform
    <$> preTransformsParser
    <*> transformsParser
    <*> postTransformsParser

preTransformsParser :: Parser Text
preTransformsParser = T.pack <$> manyTill anyChar (char '{')

transformsParser :: Parser [Transform]
transformsParser = transformParser `sepBy` char '|' <* char '}'

postTransformsParser :: Parser Text
postTransformsParser = T.pack <$> many anyChar

transformParser :: Parser Transform
transformParser = do
    result <- string "camelcase" <|> string "snakecase"
    return $ case result of
        "camelcase" -> Camelcase
        "snakecase" -> Snakecase
        _ -> Noop
