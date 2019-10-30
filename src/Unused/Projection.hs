module Unused.Projection where

import qualified Data.Bifunctor as BF
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Unused.Projection.Transform

data ParsedTransform = ParsedTransform
    { ptPre :: Text
    , ptTransforms :: [Transform]
    , ptPost :: Text
    }

type Parser = Parsec Void Text

translate :: Text -> Either String (Text -> Text)
translate template = applyTransform <$> parseTransform template

applyTransform :: ParsedTransform -> Text -> Text
applyTransform pt t = ptPre pt <> runTransformations t (ptTransforms pt) <> ptPost pt

parseTransform :: Text -> Either String ParsedTransform
parseTransform = BF.first show . parse parsedTransformParser ""

parsedTransformParser :: Parser ParsedTransform
parsedTransformParser =
    ParsedTransform <$> preTransformsParser <*> transformsParser <*> postTransformsParser

preTransformsParser :: Parser Text
preTransformsParser = T.pack <$> manyTill anySingle (char '{')

transformsParser :: Parser [Transform]
transformsParser = transformParser `sepBy` char '|' <* char '}'

postTransformsParser :: Parser Text
postTransformsParser = T.pack <$> many anySingle

transformParser :: Parser Transform
transformParser = do
    result <- string "camelcase" <|> string "snakecase"
    return $
        case result of
            "camelcase" -> Camelcase
            "snakecase" -> Snakecase
            _ -> Noop
