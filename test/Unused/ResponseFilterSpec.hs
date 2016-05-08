module Unused.ResponseFilterSpec where

import Test.Hspec
import Unused.Types (TermMatch(..), resultsFromMatches)
import Unused.ResponseFilter

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "railsSingleOkay" $ do
        it "allows controllers" $ do
            let match = TermMatch "ApplicationController" "app/controllers/application_controller.rb" 1
            let result = resultsFromMatches [match]

            railsSingleOkay result `shouldBe` True

        it "allows helpers" $ do
            let match = TermMatch "ApplicationHelper" "app/helpers/application_helper.rb" 1
            let result = resultsFromMatches [match]

            railsSingleOkay result `shouldBe` True

        it "allows migrations" $ do
            let match = TermMatch "CreateUsers" "db/migrate/20160101120000_create_users.rb" 1
            let result = resultsFromMatches [match]

            railsSingleOkay result `shouldBe` True

        it "disallows service objects" $ do
            let match = TermMatch "CreatePostWithNotifications" "app/services/create_post_with_notifications.rb" 1
            let result = resultsFromMatches [match]

            railsSingleOkay result `shouldBe` False

        it "disallows methods" $ do
            let match = TermMatch "my_method" "app/services/create_post_with_notifications.rb" 1
            let result = resultsFromMatches [match]

            railsSingleOkay result `shouldBe` False

    describe "elixirSingleOkay" $ do
        it "disallows controllers" $ do
            let match = TermMatch "PageController" "web/controllers/page_controller.rb" 1
            let result = resultsFromMatches [match]

            elixirSingleOkay result `shouldBe` False

        it "allows views" $ do
            let match = TermMatch "PageView" "web/views/page_view.rb" 1
            let result = resultsFromMatches [match]

            elixirSingleOkay result `shouldBe` True

        it "allows migrations" $ do
            let match = TermMatch "CreateUsers" "priv/repo/migrations/20160101120000_create_users.exs" 1
            let result = resultsFromMatches [match]

            elixirSingleOkay result `shouldBe` True

        it "allows tests" $ do
            let match = TermMatch "UserTest" "test/models/user_test.exs" 1
            let result = resultsFromMatches [match]

            elixirSingleOkay result `shouldBe` True


        it "disallows service modules" $ do
            let match = TermMatch "CreatePostWithNotifications" "web/services/create_post_with_notifications.ex" 1
            let result = resultsFromMatches [match]

            elixirSingleOkay result `shouldBe` False

        it "disallows functions" $ do
            let match = TermMatch "my_function" "web/services/create_post_with_notifications.ex" 1
            let result = resultsFromMatches [match]

            elixirSingleOkay result `shouldBe` False
