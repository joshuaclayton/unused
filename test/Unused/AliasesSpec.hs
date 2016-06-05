module Unused.AliasesSpec where

import Test.Hspec
import Unused.Aliases
import Unused.ResultsClassifier.Types (TermAlias(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "termsAndAliases" $ do
        it "returns the terms if no aliases are provided" $
            termsAndAliases [] ["method_1", "method_2"] `shouldBe` ["method_1", "method_2"]

        it "adds aliases to the list of terms" $ do
            let predicateAlias = TermAlias "%s?" "be_%s"
            let pluralizeAlias = TermAlias "really_%s" "very_%s"

            termsAndAliases [predicateAlias, pluralizeAlias] ["awesome?", "really_cool"]
                `shouldBe` ["awesome?", "be_awesome", "really_cool", "very_cool"]
