module Unused.TermSearch.InternalSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Unused.TermSearch.Internal
import Unused.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "commandLineOptions" $ do
        it "does not use regular expressions when the term contains non-word characters" $ do
            commandLineOptions "can_do_things?" `shouldBe` ["can_do_things?", ".", "-Q", "-c", "--ackmate", "--ignore-dir", "tmp/unused"]
            commandLineOptions "no_way!" `shouldBe` ["no_way!", ".", "-Q", "-c", "--ackmate", "--ignore-dir", "tmp/unused"]
            commandLineOptions "[]=" `shouldBe` ["[]=", ".", "-Q", "-c", "--ackmate", "--ignore-dir", "tmp/unused"]
            commandLineOptions "window.globalOverride" `shouldBe` ["window.globalOverride", ".", "-Q", "-c", "--ackmate", "--ignore-dir", "tmp/unused"]

        it "uses regular expression match with surrounding non-word matches for accuracy" $
            commandLineOptions "awesome_method" `shouldBe` ["(\\W|^)awesome_method(\\W|$)", ".", "-c", "--ackmate", "--ignore-dir", "tmp/unused"]

    describe "parseSearchResult" $ do
        it "parses normal results from `ag` to a TermMatch" $
            parseSearchResult "method_name" ":app/models/foo.rb:123" `shouldBe` (Just $ TermMatch "method_name" "app/models/foo.rb" 123)

        it "returns Nothing when it cannot parse" $
            parseSearchResult "method_name" "" `shouldBe` Nothing
