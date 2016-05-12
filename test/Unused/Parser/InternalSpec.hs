module Unused.Parser.InternalSpec where

import Test.Hspec
import Unused.Types
import Unused.Parser.Internal

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "parseTermMatch" $ do
        it "parses normal lines" $ do
            let (Right result) = parse parseTermMatch "source" ":app/files/location:12:simple\n"

            result `shouldBe` TermMatch "simple" "app/files/location" 12

        it "parses weird lines" $ do
            let (Right result) = parse parseTermMatch "source" ":app/files/location:12:Foo::Bar\n"

            result `shouldBe` TermMatch "Foo::Bar" "app/files/location" 12
