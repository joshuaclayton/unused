module Unused.Projection.Transform
    ( Transform(..)
    , runTransformations
    ) where

import           Data.Either (rights)
import           Data.Text (Text)
import qualified Text.Inflections as I
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
toCamelcase t = maybe t I.camelize $ toMaybeWords t

toSnakecase :: Text -> Text
toSnakecase t = maybe t I.underscore $ toMaybeWords t

toMaybeWords :: Text -> Maybe [I.SomeWord]
toMaybeWords t =
    U.safeHead $ rights [asCamel, asSnake]
  where
    asCamel = I.parseCamelCase [] t
    asSnake = I.parseSnakeCase [] t
