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

        it "allows matches intermixed with other results" $ do
            let appToken = TermMatch "ApplicationHelper" "app/helpers/application_helper.rb" 1
            let testToken = TermMatch "ApplicationHelper" "spec/helpers/application_helper_spec.rb" 10
            let result = resultsFromMatches [appToken, testToken]

            railsSingleOkay result `shouldBe` True

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

        it "allows Mixfile" $ do
            let match = TermMatch "Mixfile" "mix.exs" 1
            let result = resultsFromMatches [match]

            elixirSingleOkay result `shouldBe` True

        it "allows __using__" $ do
            let match = TermMatch "__using__" "web/web.ex" 1
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

        it "allows matches intermixed with other results" $ do
            let appToken = TermMatch "UserView" "web/views/user_view.ex" 1
            let testToken = TermMatch "UserView" "test/views/user_view_test.exs" 10
            let result = resultsFromMatches [appToken, testToken]

            elixirSingleOkay result `shouldBe` True

    describe "haskellSingleOkay" $ do
        it "allows instance" $ do
            let match = TermMatch "instance" "src/Lib/Types.hs" 1
            let result = resultsFromMatches [match]

            haskellSingleOkay result `shouldBe` True

        it "allows items in the *.cabal file" $ do
            let match = TermMatch "Lib.SomethingSpec" "lib.cabal" 1
            let result = resultsFromMatches [match]

            haskellSingleOkay result `shouldBe` True
