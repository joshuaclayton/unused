module Unused.ParserSpec where

import Test.Hspec
import Unused.Types (TermResults(..), TermMatch(..), RemovalLikelihood(..))
import Unused.Parser
import qualified Data.Map.Strict as Map

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "parseLines" $ do
        it "parses from the correct format" $ do
            let input = "method_name:app/path/foo.rb:1\n\
                        \other:app/path/other.rb:1\n\
                        \method_name:app/path/other.rb:5\n\
                        \method_name:spec/path/foo_spec.rb:10\n"

            let r1Matches = [ TermMatch "method_name" "app/path/foo.rb" 1
                            , TermMatch "method_name" "app/path/other.rb" 5
                            , TermMatch "method_name" "spec/path/foo_spec.rb" 10
                            ]
            let r1Results = TermResults "method_name" r1Matches 3 16 Low

            let r2Matches = [ TermMatch "other" "app/path/other.rb" 1 ]
            let r2Results = TermResults "other" r2Matches 1 1 High

            let (Right result) = parseLines input

            result `shouldBe`
                Map.fromList [ ("method_name", r1Results), ("other", r2Results) ]

        it "handles empty input" $ do
            let (Left result) = parseLines ""
            show result `shouldContain` "unexpected end of input"
