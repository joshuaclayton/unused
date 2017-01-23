module Unused.ProjectionSpec where

import Data.Text (Text)
import Test.Hspec
import Unused.Projection

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "translate" $ do
        it "replaces the text without transforms" $
            translate' "foo_{}" "bar" `shouldBe` "foo_bar"

        it "handles text transformations" $ do
            translate' "{camelcase}Validator" "proper_email" `shouldBe` "ProperEmailValidator"
            translate' "{snakecase}" "ProperEmail" `shouldBe` "proper_email"
            translate' "{camelcase}Validator" "AlreadyCamelcase" `shouldBe` "AlreadyCamelcaseValidator"

        it "handles unknown transformations" $
            translate' "{unknown}Validator" "proper_email" `shouldBe` "proper_email"

translate' :: Text -> Text -> Text
translate' template v =
    case translate template of
        Right f -> f v
        Left _ -> v
