module Unused.ResponseFilterSpec
    ( main
    , spec
    ) where

import Data.List (find)
import Test.Hspec
import Unused.ResponseFilter
import Unused.ResultsClassifier
import Unused.Types
       (TermMatch(..), TermResults, resultsFromMatches)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    parallel $ do
        describe "railsAutoLowLikelihood" $ do
            it "allows controllers" $ do
                let match =
                        TermMatch
                            "ApplicationController"
                            "app/controllers/application_controller.rb"
                            Nothing
                            1
                let result = resultsFromMatches [match]
                railsAutoLowLikelihood result `shouldBe` True
            it "allows helpers" $ do
                let match =
                        TermMatch "ApplicationHelper" "app/helpers/application_helper.rb" Nothing 1
                let result = resultsFromMatches [match]
                railsAutoLowLikelihood result `shouldBe` True
            it "allows migrations" $ do
                let match =
                        TermMatch
                            "CreateUsers"
                            "db/migrate/20160101120000_create_users.rb"
                            Nothing
                            1
                let result = resultsFromMatches [match]
                railsAutoLowLikelihood result `shouldBe` True
            it "disallows service objects" $ do
                let match =
                        TermMatch
                            "CreatePostWithNotifications"
                            "app/services/create_post_with_notifications.rb"
                            Nothing
                            1
                let result = resultsFromMatches [match]
                railsAutoLowLikelihood result `shouldBe` False
            it "disallows methods" $ do
                let match =
                        TermMatch
                            "my_method"
                            "app/services/create_post_with_notifications.rb"
                            Nothing
                            1
                let result = resultsFromMatches [match]
                railsAutoLowLikelihood result `shouldBe` False
            it "disallows models that occur in migrations" $ do
                let model = TermMatch "User" "app/models/user.rb" Nothing 1
                let migration =
                        TermMatch "User" "db/migrate/20160101120000_create_users.rb" Nothing 1
                let result = resultsFromMatches [model, migration]
                railsAutoLowLikelihood result `shouldBe` False
            it "allows matches intermixed with other results" $ do
                let appToken =
                        TermMatch "ApplicationHelper" "app/helpers/application_helper.rb" Nothing 1
                let testToken =
                        TermMatch
                            "ApplicationHelper"
                            "spec/helpers/application_helper_spec.rb"
                            Nothing
                            10
                let result = resultsFromMatches [appToken, testToken]
                railsAutoLowLikelihood result `shouldBe` True
        describe "elixirAutoLowLikelihood" $ do
            it "disallows controllers" $ do
                let match =
                        TermMatch "PageController" "web/controllers/page_controller.rb" Nothing 1
                let result = resultsFromMatches [match]
                elixirAutoLowLikelihood result `shouldBe` False
            it "allows views" $ do
                let match = TermMatch "PageView" "web/views/page_view.rb" Nothing 1
                let result = resultsFromMatches [match]
                elixirAutoLowLikelihood result `shouldBe` True
            it "allows migrations" $ do
                let match =
                        TermMatch
                            "CreateUsers"
                            "priv/repo/migrations/20160101120000_create_users.exs"
                            Nothing
                            1
                let result = resultsFromMatches [match]
                elixirAutoLowLikelihood result `shouldBe` True
            it "allows tests" $ do
                let match = TermMatch "UserTest" "test/models/user_test.exs" Nothing 1
                let result = resultsFromMatches [match]
                elixirAutoLowLikelihood result `shouldBe` True
            it "allows Mixfile" $ do
                let match = TermMatch "Mixfile" "mix.exs" Nothing 1
                let result = resultsFromMatches [match]
                elixirAutoLowLikelihood result `shouldBe` True
            it "allows __using__" $ do
                let match = TermMatch "__using__" "web/web.ex" Nothing 1
                let result = resultsFromMatches [match]
                elixirAutoLowLikelihood result `shouldBe` True
            it "disallows service modules" $ do
                let match =
                        TermMatch
                            "CreatePostWithNotifications"
                            "web/services/create_post_with_notifications.ex"
                            Nothing
                            1
                let result = resultsFromMatches [match]
                elixirAutoLowLikelihood result `shouldBe` False
            it "disallows functions" $ do
                let match =
                        TermMatch
                            "my_function"
                            "web/services/create_post_with_notifications.ex"
                            Nothing
                            1
                let result = resultsFromMatches [match]
                elixirAutoLowLikelihood result `shouldBe` False
            it "allows matches intermixed with other results" $ do
                let appToken = TermMatch "UserView" "web/views/user_view.ex" Nothing 1
                let testToken = TermMatch "UserView" "test/views/user_view_test.exs" Nothing 10
                let result = resultsFromMatches [appToken, testToken]
                elixirAutoLowLikelihood result `shouldBe` True
        describe "haskellAutoLowLikelihood" $ do
            it "allows instance" $ do
                let match = TermMatch "instance" "src/Lib/Types.hs" Nothing 1
                let result = resultsFromMatches [match]
                haskellAutoLowLikelihood result `shouldBe` True
            it "allows items in the *.cabal file" $ do
                let match = TermMatch "Lib.SomethingSpec" "lib.cabal" Nothing 1
                let result = resultsFromMatches [match]
                haskellAutoLowLikelihood result `shouldBe` True
        describe "autoLowLikelihood" $
            it "doesn't qualify as low when no matchers are present in a language config" $ do
                let match = TermMatch "AwesomeThing" "app/foo/awesome_thing.rb" Nothing 1
                let result = resultsFromMatches [match]
                let languageConfig =
                        LanguageConfiguration
                            "Bad"
                            []
                            [LowLikelihoodMatch "Match with empty matchers" [] False]
                            []
                autoLowLikelihood languageConfig result `shouldBe` False

configByName :: String -> LanguageConfiguration
configByName s = config'
  where
    (Right config) = loadConfig
    (Just config') = find ((==) s . lcName) config

railsAutoLowLikelihood :: TermResults -> Bool
railsAutoLowLikelihood = autoLowLikelihood (configByName "Rails")

elixirAutoLowLikelihood :: TermResults -> Bool
elixirAutoLowLikelihood = autoLowLikelihood (configByName "Phoenix")

haskellAutoLowLikelihood :: TermResults -> Bool
haskellAutoLowLikelihood = autoLowLikelihood (configByName "Haskell")
