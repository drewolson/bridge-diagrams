module Bridge.Data.UnknownSpec
  ( spec,
  )
where

import Bridge.Data.Rank (Rank (..))
import Bridge.Data.Unknown qualified as Unknown
import Test.Hspec (Spec, describe, it, parallel, shouldBe)

spec :: Spec
spec = parallel do
  describe "areAllKnown" do
    it "returns true if all entries are known" do
      Unknown.areAllKnown [Ace, King, Queen] `shouldBe` True

    it "returns true if any entries are unknown" do
      Unknown.areAllKnown [Ace, King, Queen, Unknown] `shouldBe` False

  describe "knowns" do
    it "returns true if any entries are unknown" do
      Unknown.knowns [Ace, King, Queen, Unknown] `shouldBe` [Ace, King, Queen]

  describe "hasUniqueEntries" do
    it "returns true if the entries are unique" do
      Unknown.hasUniqueEntries [Ace, King, Queen] `shouldBe` True

    it "returns ignores unknown entries" do
      Unknown.hasUniqueEntries [Ace, King, Queen, Unknown, Unknown] `shouldBe` True

  describe "duplicates" do
    it "returns duplicates, ignoring unknowns" do
      Unknown.duplicates [Ace, Ace, Unknown, Unknown] `shouldBe` [Ace]

    it "returns a single entry per duplicate" do
      Unknown.duplicates [Ace, Ace, Ace, Unknown, Unknown] `shouldBe` [Ace]

  describe "showDuplicates" do
    it "displays the duplicates in a list" do
      Unknown.showDuplicates [Ace, Ace, King, King, Unknown, Unknown] `shouldBe` "A, K"
