module Bridge.Data.LayoutSpec
  ( spec,
  )
where

import Bridge.Data.Layout (Layout (..))
import Bridge.Data.Layout qualified as Layout
import Bridge.Data.Rank (Rank (..))
import Test.Hspec (Spec, describe, it, parallel, shouldBe)

spec :: Spec
spec = parallel do
  describe "suitCombination" do
    it "returns a suit combination when given valid ranks" do
      let expected =
            SuitCombination
              { top = [Ace, King, Unknown, Unknown, Unknown],
                bottom = [Queen, Ten, Unknown]
              }

      let result =
            Layout.suitCombination
              [Ace, King, Unknown, Unknown, Unknown]
              [Queen, Ten, Unknown]

      result `shouldBe` Right expected

    it "does not allow repeated ranks" do
      let result =
            Layout.suitCombination
              [Ace, King, Unknown, Unknown, Unknown]
              [Ace, Ten, Unknown]

      result `shouldBe` Left "Suit combination has duplicate ranks: A"

    it "does not allow more than 13 cards" do
      let result =
            Layout.suitCombination
              [Ace, King, Unknown, Unknown, Unknown, Unknown, Unknown]
              [Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown]

      result `shouldBe` Left "Suit combination cannot contain more than 13 cards"
