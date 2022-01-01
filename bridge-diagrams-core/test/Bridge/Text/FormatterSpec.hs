module Bridge.Text.FormatterSpec
  ( spec,
  )
where

import Bridge.Data.Card (Card (..))
import Bridge.Data.Diagram (Diagram (..))
import Bridge.Data.Layout (Layout (..))
import Bridge.Data.Perspective (Perspective (..))
import Bridge.Data.Rank (Rank (..))
import Bridge.Data.Scoring (Scoring (Imps))
import Bridge.Data.Suit (Suit (..))
import Bridge.Data.Vul (Vul (..))
import Bridge.Text.Formatter qualified as Formatter
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec (Spec, describe, it, parallel, shouldBe)
import Text.RawString.QQ (r)

trimLines :: Text -> Text
trimLines = Text.unlines . fmap Text.stripEnd . Text.lines

spec :: Spec
spec = parallel do
  describe "format" do
    it "formats a suit combination" do
      let result =
            Formatter.format
              Diagram
                { layout =
                    SuitCombination
                      { top = [Ace, King, Unknown, Unknown, Unknown],
                        bottom = [Queen, Ten, Unknown]
                      },
                  vul = Nothing,
                  lead = Nothing,
                  scoring = Nothing
                }

      let expected =
            [r|
AKxxx
-----
QTx
|]

      trimLines result `shouldBe` Text.drop 1 expected

    it "formats a single hand" do
      let result =
            Formatter.format
              Diagram
                { layout =
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
                      },
                  vul = Nothing,
                  lead = Nothing,
                  scoring = Nothing
                }

      let expected =
            [r|
♠AKxxx
♥KQx
♦Txx
♣xx
|]

      trimLines result `shouldBe` Text.drop 1 expected

    it "formats a double-dummy hand" do
      let result =
            Formatter.format
              Diagram
                { layout =
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
                      },
                  vul = Nothing,
                  lead = Nothing,
                  scoring = Nothing
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

    it "formats a double-dummy hand with vul, scoring, and opening lead" do
      let result =
            Formatter.format
              Diagram
                { layout =
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
                      },
                  vul = Just RR,
                  lead = Just $ Card Clubs Ten,
                  scoring = Just Imps
                }

      let expected =
            [r|
Vul: R/R   ♠AKxxx
IMPs       ♥Qxx
           ♦JTx
           ♣xx
♠xxx        -----     ♠Qxx
♥xxx       |  N  |    ♥AKxxx
♦Qx        |W   E|    ♦xxx
♣Axxxx     |  S  |    ♣Kx
            -----
Lead: ♣T   ♠Jx
           ♥Jx
           ♦AKxxx
           ♣Qxxx
|]

      trimLines result `shouldBe` Text.drop 1 expected

    it "formats a single-dummy hand" do
      let result =
            Formatter.format
              Diagram
                { layout =
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
                      },
                  vul = Nothing,
                  lead = Nothing,
                  scoring = Nothing
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

    it "formats a single-dummy hand with vul and opening lead" do
      let result =
            Formatter.format
              Diagram
                { layout =
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
                      },
                  vul = Just RR,
                  lead = Just $ Card Diamonds Four,
                  scoring = Nothing
                }

      let expected =
            [r|
Vul: R/R   ♠AKxxx
           ♥Qxx
           ♦JTx
           ♣xx
Lead: ♦4
           ♠Jx
           ♥Jx
           ♦AKxxx
           ♣Qxxx
|]

      trimLines result `shouldBe` Text.drop 1 expected

    it "formats a defensive hand from the west perspective" do
      let result =
            Formatter.format
              Diagram
                { layout =
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
                      },
                  vul = Nothing,
                  lead = Nothing,
                  scoring = Nothing
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

    it "formats a defensive hand from the west perspective with vul" do
      let result =
            Formatter.format
              Diagram
                { layout =
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
                      },
                  vul = Just RR,
                  lead = Nothing,
                  scoring = Nothing
                }

      let expected =
            [r|
Vul: R/R   ♠AKxxx
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
              Diagram
                { layout =
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
                      },
                  vul = Nothing,
                  lead = Nothing,
                  scoring = Nothing
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

    it "formats a defensive hand from the east perspective with vul and opening lead" do
      let result =
            Formatter.format
              Diagram
                { layout =
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
                      },
                  vul = Just RR,
                  lead = Just $ Card Diamonds Four,
                  scoring = Nothing
                }

      let expected =
            [r|
Vul: R/R   ♠AKxxx
           ♥Qxx
           ♦JTx
           ♣xx
Lead: ♦4    -----     ♠Qxx
           |  N  |    ♥AKxxx
           |    E|    ♦xxx
           |     |    ♣Kx
            -----
|]

      trimLines result `shouldBe` Text.drop 1 expected

    it "formats a defensive hand from the east perspective with vul, opening lead, and scoring" do
      let result =
            Formatter.format
              Diagram
                { layout =
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
                      },
                  vul = Just RR,
                  lead = Just $ Card Diamonds Four,
                  scoring = Just Imps
                }

      let expected =
            [r|
Vul: R/R   ♠AKxxx
IMPs       ♥Qxx
           ♦JTx
           ♣xx
Lead: ♦4    -----     ♠Qxx
           |  N  |    ♥AKxxx
           |    E|    ♦xxx
           |     |    ♣Kx
            -----
|]

      trimLines result `shouldBe` Text.drop 1 expected
