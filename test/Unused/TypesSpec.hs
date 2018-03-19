module Unused.TypesSpec where

import Test.Hspec
import Unused.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    parallel $
    describe "resultsFromMatches" $
    it "batches files together to calculate information" $ do
        let matches =
                [ TermMatch
                      "ApplicationController"
                      "app/controllers/application_controller.rb"
                      Nothing
                      1
                , TermMatch
                      "ApplicationController"
                      "spec/controllers/application_controller_spec.rb"
                      Nothing
                      10
                ]
        resultsFromMatches matches `shouldBe`
            TermResults
                "ApplicationController"
                ["ApplicationController"]
                matches
                (Occurrences 1 10)
                (Occurrences 1 1)
                (Occurrences 2 11)
                (Removal NotCalculated "Likelihood not calculated")
                Nothing
