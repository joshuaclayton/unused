module Unused.ParserSpec where

import Test.Hspec
import Unused.Types
import Unused.Parser
import qualified Data.Map.Strict as Map

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "parseLines" $ do
        it "parses from the correct format" $ do
            let input = ":app/path/foo.rb:1:method_name\n\
                        \:app/path/other.rb:1:other\n\
                        \:app/path/other.rb:5:method_name\n\
                        \:spec/path/foo_spec.rb:10:method_name\n"

            let r1Matches = [ TermMatch "method_name" "app/path/foo.rb" 1
                            , TermMatch "method_name" "app/path/other.rb" 5
                            , TermMatch "method_name" "spec/path/foo_spec.rb" 10
                            ]
            let r1Results = TermResults "method_name" r1Matches (Occurrences 1 10) (Occurrences 2 6) (Occurrences 3 16) (Removal Low "used frequently")

            let r2Matches = [ TermMatch "other" "app/path/other.rb" 1 ]
            let r2Results = TermResults "other" r2Matches (Occurrences 0 0) (Occurrences 1 1) (Occurrences 1 1) (Removal High "used infrequently")

            let (Right result) = parseLines input

            result `shouldBe`
                Map.fromList [ ("method_name", r1Results), ("other", r2Results) ]

        it "handles empty input" $ do
            let (Left result) = parseLines ""
            show result `shouldContain` "unexpected end of input"
