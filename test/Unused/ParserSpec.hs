module Unused.ParserSpec where

import qualified Data.Map.Strict as Map
import           Test.Hspec
import           Unused.Parser
import           Unused.ResultsClassifier
import           Unused.TermSearch
import           Unused.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "parseResults" $ do
        it "parses from the correct format" $ do
            let r1Matches = [ TermMatch "method_name" "app/path/foo.rb" 1
                            , TermMatch "method_name" "app/path/other.rb" 5
                            , TermMatch "method_name" "spec/path/foo_spec.rb" 10
                            ]
            let r1Results = TermResults "method_name" ["method_name"] r1Matches (Occurrences 1 10) (Occurrences 2 6) (Occurrences 3 16) (Removal Low "used frequently") Nothing

            let r2Matches = [ TermMatch "other" "app/path/other.rb" 1 ]
            let r2Results = TermResults "other" ["other"] r2Matches (Occurrences 0 0) (Occurrences 1 1) (Occurrences 1 1) (Removal High "used once") Nothing

            (Right config) <- loadConfig

            let result = parseResults config $ SearchResults $ r1Matches ++ r2Matches

            result `shouldBe`
                Map.fromList [ ("method_name", r1Results), ("other", r2Results) ]

        it "parses when no config is provided" $ do
            let r1Matches = [ TermMatch "method_name" "app/path/foo.rb" 1
                            , TermMatch "method_name" "app/path/other.rb" 5
                            , TermMatch "method_name" "spec/path/foo_spec.rb" 10
                            ]
            let r1Results = TermResults "method_name" ["method_name"] r1Matches (Occurrences 1 10) (Occurrences 2 6) (Occurrences 3 16) (Removal Low "used frequently") Nothing

            let result = parseResults [] $ SearchResults r1Matches

            result `shouldBe`
                Map.fromList [ ("method_name", r1Results) ]

        it "handles aliases correctly" $ do
            let r1Matches = [ TermMatch "admin?" "app/path/user.rb" 3 ]

            let r2Matches = [ TermMatch "be_admin" "spec/models/user_spec.rb" 2
                            , TermMatch "be_admin" "spec/features/user_promoted_to_admin_spec.rb" 2
                            ]


            (Right config) <- loadConfig
            let searchResults = r1Matches ++ r2Matches

            let result = parseResults config $ SearchResults searchResults

            let results = TermResults "admin?" ["admin?", "be_admin"] searchResults (Occurrences 2 4) (Occurrences 1 3) (Occurrences 3 7) (Removal Low "used frequently") Nothing
            result `shouldBe`
                Map.fromList [ ("admin?|be_admin", results) ]

        it "handles empty input" $ do
            (Right config) <- loadConfig
            let result = parseResults config $ SearchResults []
            result `shouldBe` Map.fromList []
