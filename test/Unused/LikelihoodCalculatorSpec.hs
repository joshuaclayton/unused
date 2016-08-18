module Unused.LikelihoodCalculatorSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Unused.LikelihoodCalculator
import Unused.ResultsClassifier
import Unused.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "calculateLikelihood" $ do
        it "prefers language-specific checks first" $ do
            let railsMatches = [ TermMatch "ApplicationController" "app/controllers/application_controller.rb" Nothing 1 ]
            removalLikelihood' railsMatches `shouldBe` Low

            let elixirMatches = [ TermMatch "AwesomeView" "web/views/awesome_view.ex" Nothing 1 ]
            removalLikelihood' elixirMatches `shouldBe` Low

        it "weighs widely-used methods as low likelihood" $ do
            let matches = [ TermMatch "full_name" "app/models/user.rb" Nothing 4
                          , TermMatch "full_name" "app/views/application/_auth_header.rb" Nothing 1
                          , TermMatch "full_name" "app/mailers/user_mailer.rb" Nothing 1
                          , TermMatch "full_name" "spec/models/user_spec.rb" Nothing 10
                          ]

            removalLikelihood' matches `shouldBe` Low

        it "weighs only-occurs-once methods as high likelihood" $ do
            let matches = [ TermMatch "obscure_method" "app/models/user.rb" Nothing 1 ]

            removalLikelihood' matches `shouldBe` High

        it "weighs methods that seem to only be tested and never used as high likelihood" $ do
            let matches = [ TermMatch "obscure_method" "app/models/user.rb" Nothing 1
                          , TermMatch "obscure_method" "spec/models/user_spec.rb" Nothing 5
                          ]

            removalLikelihood' matches `shouldBe` High

        it "weighs methods that seem to only be tested and used in one other area as medium likelihood" $ do
            let matches = [ TermMatch "obscure_method" "app/models/user.rb" Nothing 1
                          , TermMatch "obscure_method" "app/controllers/user_controller.rb" Nothing 1
                          , TermMatch "obscure_method" "spec/models/user_spec.rb" Nothing 5
                          , TermMatch "obscure_method" "spec/controllers/user_controller_spec.rb" Nothing 5
                          ]

            removalLikelihood' matches `shouldBe` Medium

        it "doesn't mis-categorize allowed terms from different languages" $ do
            let matches = [ TermMatch "t" "web/models/foo.ex" Nothing 1 ]

            removalLikelihood' matches `shouldBe` High

removalLikelihood' :: [TermMatch] -> RemovalLikelihood
removalLikelihood' =
    rLikelihood . trRemoval . calculateLikelihood config . resultsFromMatches
  where
    (Right config) = loadConfig
