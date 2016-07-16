module Unused.Projection.Transform
    ( Transform(..)
    , runTransformations
    ) where

import           Data.Either (rights)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Inflections as I
import qualified Text.Inflections.Parse.Types as I
import qualified Unused.Util as U

data Transform
    = Camelcase
    | Snakecase
    | Noop

runTransformations :: Text -> [Transform] -> Text
runTransformations = foldl (flip runTransformation)

runTransformation :: Transform -> Text -> Text
runTransformation Camelcase = toCamelcase
runTransformation Snakecase = toSnakecase
runTransformation Noop = id

toCamelcase :: Text -> Text
toCamelcase t = maybe t (T.pack . I.camelize) $ toMaybeWords t

toSnakecase :: Text -> Text
toSnakecase t = maybe t (T.pack . I.underscore) $ toMaybeWords t

toMaybeWords :: Text -> Maybe [I.Word]
toMaybeWords t =
    U.safeHead $ rights [asCamel, asSnake]
  where
    asCamel = I.parseCamelCase [] $ T.unpack t
    asSnake = I.parseSnakeCase [] $ T.unpack t
