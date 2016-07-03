module Unused.Grouping.InternalSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Unused.Grouping.Internal
import Unused.Grouping.Types
import Unused.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "groupFilter" $ do
        it "groups by directory" $ do
            let termMatch = TermMatch "AwesomeClass" "foo/bar/baz/buzz.rb" 10

            groupFilter GroupByDirectory termMatch `shouldBe` ByDirectory "foo/bar"

        it "groups by term" $ do
            let termMatch = TermMatch "AwesomeClass" "foo/bar/baz/buzz.rb" 10

            groupFilter GroupByTerm termMatch `shouldBe` ByTerm "AwesomeClass"

        it "groups by file" $ do
            let termMatch = TermMatch "AwesomeClass" "foo/bar/baz/buzz.rb" 10

            groupFilter GroupByFile termMatch `shouldBe` ByFile "foo/bar/baz/buzz.rb"

        it "groups by nothing" $ do
            let termMatch = TermMatch "AwesomeClass" "foo/bar/baz/buzz.rb" 10

            groupFilter NoGroup termMatch `shouldBe` NoGrouping
