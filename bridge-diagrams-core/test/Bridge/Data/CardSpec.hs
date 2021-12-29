module Bridge.Data.CardSpec
  ( spec,
  )
where

import Bridge.Data.Card (Card (..))
import Bridge.Data.Card qualified as Card
import Bridge.Data.Rank (Rank (..))
import Bridge.Data.Suit (Suit (..))
import Test.Hspec (Spec, describe, it, parallel, shouldBe)

spec :: Spec
spec = parallel do
  describe "enumerate" do
    it "returns all cards" do
      Card.enumerate
        `shouldBe` [ Card Spades Ace,
                     Card Spades King,
                     Card Spades Queen,
                     Card Spades Jack,
                     Card Spades Ten,
                     Card Spades Nine,
                     Card Spades Eight,
                     Card Spades Seven,
                     Card Spades Six,
                     Card Spades Five,
                     Card Spades Four,
                     Card Spades Three,
                     Card Spades Two,
                     Card Hearts Ace,
                     Card Hearts King,
                     Card Hearts Queen,
                     Card Hearts Jack,
                     Card Hearts Ten,
                     Card Hearts Nine,
                     Card Hearts Eight,
                     Card Hearts Seven,
                     Card Hearts Six,
                     Card Hearts Five,
                     Card Hearts Four,
                     Card Hearts Three,
                     Card Hearts Two,
                     Card Diamonds Ace,
                     Card Diamonds King,
                     Card Diamonds Queen,
                     Card Diamonds Jack,
                     Card Diamonds Ten,
                     Card Diamonds Nine,
                     Card Diamonds Eight,
                     Card Diamonds Seven,
                     Card Diamonds Six,
                     Card Diamonds Five,
                     Card Diamonds Four,
                     Card Diamonds Three,
                     Card Diamonds Two,
                     Card Clubs Ace,
                     Card Clubs King,
                     Card Clubs Queen,
                     Card Clubs Jack,
                     Card Clubs Ten,
                     Card Clubs Nine,
                     Card Clubs Eight,
                     Card Clubs Seven,
                     Card Clubs Six,
                     Card Clubs Five,
                     Card Clubs Four,
                     Card Clubs Three,
                     Card Clubs Two
                   ]

  describe "honors" do
    it "returns all honors" do
      Card.honors
        `shouldBe` [ Card Spades Ace,
                     Card Spades King,
                     Card Spades Queen,
                     Card Spades Jack,
                     Card Spades Ten,
                     Card Hearts Ace,
                     Card Hearts King,
                     Card Hearts Queen,
                     Card Hearts Jack,
                     Card Hearts Ten,
                     Card Diamonds Ace,
                     Card Diamonds King,
                     Card Diamonds Queen,
                     Card Diamonds Jack,
                     Card Diamonds Ten,
                     Card Clubs Ace,
                     Card Clubs King,
                     Card Clubs Queen,
                     Card Clubs Jack,
                     Card Clubs Ten
                   ]
