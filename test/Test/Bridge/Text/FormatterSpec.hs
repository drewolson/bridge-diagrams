module Test.Bridge.Text.FormatterSpec
  ( spec,
  )
where

import Bridge.Data.Card (Card (..))
import Bridge.Data.Diagram (Diagram (..))
import Bridge.Data.Perspective (Perspective (..))
import Bridge.Data.Rank (Rank (..))
import Bridge.Data.Suit (Suit (..))
import Bridge.Text.Formatter qualified as Formatter
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec (Spec, describe, it, parallel, shouldBe)
import Text.RawString.QQ (r)

trimLines :: Text -> Text
trimLines = Text.unlines . fmap Text.stripEnd . Text.lines

spec :: Spec
spec = parallel do
  describe "Bridge.Text.Formatter" do
    describe "format" do
      it "formats a single hand" do
        let result =
              Formatter.format
                SingleHand
                  { hand =
                      [ Card Spades Ace,
                        Card Spades King,
                        Card Spades Unknown,
                        Card Spades Unknown,
                        Card Spades Unknown,
                        Card Hearts King,
                        Card Hearts Queen,
                        Card Hearts Unknown,
                        Card Diamonds Ten,
                        Card Diamonds Unknown,
                        Card Diamonds Unknown,
                        Card Clubs Unknown,
                        Card Clubs Unknown
                      ]
                  }

        let expected =
              [r|
♠AKxxx
♥KQx
♦Txx
♣xx|]

        result `shouldBe` Text.drop 1 expected

      it "formats a double-dummy hand" do
        let result =
              Formatter.format
                DoubleDummy
                  { north =
                      [ Card Spades Ace,
                        Card Spades King,
                        Card Spades Unknown,
                        Card Spades Unknown,
                        Card Spades Unknown,
                        Card Hearts Queen,
                        Card Hearts Unknown,
                        Card Hearts Unknown,
                        Card Diamonds Jack,
                        Card Diamonds Ten,
                        Card Diamonds Unknown,
                        Card Clubs Unknown,
                        Card Clubs Unknown
                      ],
                    east =
                      [ Card Spades Queen,
                        Card Spades Unknown,
                        Card Spades Unknown,
                        Card Hearts Ace,
                        Card Hearts King,
                        Card Hearts Unknown,
                        Card Hearts Unknown,
                        Card Hearts Unknown,
                        Card Diamonds Unknown,
                        Card Diamonds Unknown,
                        Card Diamonds Unknown,
                        Card Clubs King,
                        Card Clubs Unknown
                      ],
                    south =
                      [ Card Spades Jack,
                        Card Spades Unknown,
                        Card Hearts Jack,
                        Card Hearts Unknown,
                        Card Diamonds Ace,
                        Card Diamonds King,
                        Card Diamonds Unknown,
                        Card Diamonds Unknown,
                        Card Diamonds Unknown,
                        Card Clubs Queen,
                        Card Clubs Unknown,
                        Card Clubs Unknown,
                        Card Clubs Unknown
                      ],
                    west =
                      [ Card Spades Unknown,
                        Card Spades Unknown,
                        Card Spades Unknown,
                        Card Hearts Unknown,
                        Card Hearts Unknown,
                        Card Hearts Unknown,
                        Card Diamonds Queen,
                        Card Diamonds Unknown,
                        Card Clubs Ace,
                        Card Clubs Unknown,
                        Card Clubs Unknown,
                        Card Clubs Unknown,
                        Card Clubs Unknown
                      ]
                  }

        let expected =
              [r|
           ♠AKxxx
           ♥Qxx
           ♦JTx
           ♣xx
♠xxx        -----     ♠Qxx
♥xxx       |  N  |    ♥AKxxx
♦Qx        |W   E|    ♦xxx
♣Axxxx     |  S  |    ♣Kx
            -----
           ♠Jx
           ♥Jx
           ♦AKxxx
           ♣Qxxx
|]

        trimLines result `shouldBe` Text.drop 1 expected

      it "formats a single-dummy hand" do
        let result =
              Formatter.format
                SingleDummy
                  { north =
                      [ Card Spades Ace,
                        Card Spades King,
                        Card Spades Unknown,
                        Card Spades Unknown,
                        Card Spades Unknown,
                        Card Hearts Queen,
                        Card Hearts Unknown,
                        Card Hearts Unknown,
                        Card Diamonds Jack,
                        Card Diamonds Ten,
                        Card Diamonds Unknown,
                        Card Clubs Unknown,
                        Card Clubs Unknown
                      ],
                    south =
                      [ Card Spades Jack,
                        Card Spades Unknown,
                        Card Hearts Jack,
                        Card Hearts Unknown,
                        Card Diamonds Ace,
                        Card Diamonds King,
                        Card Diamonds Unknown,
                        Card Diamonds Unknown,
                        Card Diamonds Unknown,
                        Card Clubs Queen,
                        Card Clubs Unknown,
                        Card Clubs Unknown,
                        Card Clubs Unknown
                      ]
                  }

        let expected =
              [r|
♠AKxxx
♥Qxx
♦JTx
♣xx

♠Jx
♥Jx
♦AKxxx
♣Qxxx
|]

        trimLines result `shouldBe` Text.drop 1 expected

      it "formats a defensive hand from the west perspective" do
        let result =
              Formatter.format
                Defense
                  { perspective = West,
                    dummy =
                      [ Card Spades Ace,
                        Card Spades King,
                        Card Spades Unknown,
                        Card Spades Unknown,
                        Card Spades Unknown,
                        Card Hearts Queen,
                        Card Hearts Unknown,
                        Card Hearts Unknown,
                        Card Diamonds Jack,
                        Card Diamonds Ten,
                        Card Diamonds Unknown,
                        Card Clubs Unknown,
                        Card Clubs Unknown
                      ],
                    defender =
                      [ Card Spades Unknown,
                        Card Spades Unknown,
                        Card Spades Unknown,
                        Card Hearts Unknown,
                        Card Hearts Unknown,
                        Card Hearts Unknown,
                        Card Diamonds Queen,
                        Card Diamonds Unknown,
                        Card Clubs Ace,
                        Card Clubs Unknown,
                        Card Clubs Unknown,
                        Card Clubs Unknown,
                        Card Clubs Unknown
                      ]
                  }

        let expected =
              [r|
           ♠AKxxx
           ♥Qxx
           ♦JTx
           ♣xx
♠xxx        -----
♥xxx       |  N  |
♦Qx        |W    |
♣Axxxx     |     |
            -----
|]

        trimLines result `shouldBe` Text.drop 1 expected

      it "formats a defensive hand from the east perspective" do
        let result =
              Formatter.format
                Defense
                  { perspective = East,
                    dummy =
                      [ Card Spades Ace,
                        Card Spades King,
                        Card Spades Unknown,
                        Card Spades Unknown,
                        Card Spades Unknown,
                        Card Hearts Queen,
                        Card Hearts Unknown,
                        Card Hearts Unknown,
                        Card Diamonds Jack,
                        Card Diamonds Ten,
                        Card Diamonds Unknown,
                        Card Clubs Unknown,
                        Card Clubs Unknown
                      ],
                    defender =
                      [ Card Spades Queen,
                        Card Spades Unknown,
                        Card Spades Unknown,
                        Card Hearts Ace,
                        Card Hearts King,
                        Card Hearts Unknown,
                        Card Hearts Unknown,
                        Card Hearts Unknown,
                        Card Diamonds Unknown,
                        Card Diamonds Unknown,
                        Card Diamonds Unknown,
                        Card Clubs King,
                        Card Clubs Unknown
                      ]
                  }

        let expected =
              [r|
♠AKxxx
♥Qxx
♦JTx
♣xx
 -----     ♠Qxx
|  N  |    ♥AKxxx
|    E|    ♦xxx
|     |    ♣Kx
 -----
|]

        trimLines result `shouldBe` Text.drop 1 expected
