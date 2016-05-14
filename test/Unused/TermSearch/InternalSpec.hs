module Unused.TermSearch.InternalSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Unused.TermSearch.Internal

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "commandLineOptions" $ do
        it "does not use regular expressions when the term contains non-word characters" $ do
            commandLineOptions "can_do_things?" `shouldBe` ["can_do_things?", ".", "-c", "-Q", "--ackmate"]
            commandLineOptions "no_way!" `shouldBe` ["no_way!", ".", "-c", "-Q", "--ackmate"]
            commandLineOptions "[]=" `shouldBe` ["[]=", ".", "-c", "-Q", "--ackmate"]
            commandLineOptions "window.globalOverride" `shouldBe` ["window.globalOverride", ".", "-c", "-Q", "--ackmate"]

        it "uses regular expression match with surrounding non-word matches for accuracy" $
            commandLineOptions "awesome_method" `shouldBe` ["\\Wawesome_method\\W", ".", "-c", "--ackmate"]
