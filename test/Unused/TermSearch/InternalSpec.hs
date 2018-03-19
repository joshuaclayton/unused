module Unused.TermSearch.InternalSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Unused.TermSearch.Internal
import Unused.TermSearch.Types
import Unused.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    parallel $ do
        describe "commandLineOptions" $ do
            it "does not use regular expressions when the term contains non-word characters" $ do
                commandLineOptions Ag "can_do_things?" `shouldBe`
                    ["can_do_things?", ".", "-Q", "-c", "--ackmate", "--ignore-dir", "tmp/unused"]
                commandLineOptions Ag "no_way!" `shouldBe`
                    ["no_way!", ".", "-Q", "-c", "--ackmate", "--ignore-dir", "tmp/unused"]
                commandLineOptions Ag "[]=" `shouldBe`
                    ["[]=", ".", "-Q", "-c", "--ackmate", "--ignore-dir", "tmp/unused"]
                commandLineOptions Ag "window.globalOverride" `shouldBe`
                    [ "window.globalOverride"
                    , "."
                    , "-Q"
                    , "-c"
                    , "--ackmate"
                    , "--ignore-dir"
                    , "tmp/unused"
                    ]
                commandLineOptions Rg "can_do_things?" `shouldBe`
                    ["can_do_things?", ".", "-F", "-c", "-j", "1"]
                commandLineOptions Rg "no_way!" `shouldBe` ["no_way!", ".", "-F", "-c", "-j", "1"]
                commandLineOptions Rg "[]=" `shouldBe` ["[]=", ".", "-F", "-c", "-j", "1"]
                commandLineOptions Rg "window.globalOverride" `shouldBe`
                    ["window.globalOverride", ".", "-F", "-c", "-j", "1"]
            it "uses regular expression match with surrounding non-word matches for accuracy" $ do
                commandLineOptions Ag "awesome_method" `shouldBe`
                    [ "(\\W|^)awesome_method(\\W|$)"
                    , "."
                    , "-c"
                    , "--ackmate"
                    , "--ignore-dir"
                    , "tmp/unused"
                    ]
                commandLineOptions Rg "awesome_method" `shouldBe`
                    ["(\\W|^)awesome_method(\\W|$)", ".", "-c", "-j", "1"]
        describe "parseSearchResult" $ do
            it "parses normal results from `ag` to a TermMatch" $ do
                parseSearchResult Ag (OriginalTerm "method_name") ":app/models/foo.rb:123" `shouldBe`
                    (Just $ TermMatch "method_name" "app/models/foo.rb" Nothing 123)
                parseSearchResult Rg (OriginalTerm "method_name") "app/models/foo.rb:123" `shouldBe`
                    (Just $ TermMatch "method_name" "app/models/foo.rb" Nothing 123)
            it "returns Nothing when it cannot parse" $ do
                parseSearchResult Ag (OriginalTerm "method_name") "" `shouldBe` Nothing
                parseSearchResult Rg (OriginalTerm "method_name") "" `shouldBe` Nothing
