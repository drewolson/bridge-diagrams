module Bridge.Text.ParserSpec
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
import Bridge.Text.Parser qualified as Parser
import Test.Hspec (Spec, describe, it, parallel, shouldBe)

spec :: Spec
spec = parallel do
  describe "parse" do
    it "parses a single hand" do
      let result = Parser.parse "akxxx kqx txx xx"
      let expected =
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

      result `shouldBe` Right expected

    it "parses partial hands with empty suits" do
      let result = Parser.parse "akxxx - - -"
      let expected =
            Diagram
              { layout =
                  SingleHand
                    { hand =
                        [ Card Spades Ace,
                          Card Spades King,
                          Card Spades Unknown,
                          Card Spades Unknown,
                          Card Spades Unknown
                        ]
                    },
                vul = Nothing,
                lead = Nothing,
                scoring = Nothing
              }

      result `shouldBe` Right expected

    it "parses empty suits with void or Void" do
      let result = Parser.parse "akxxx void Void -"
      let expected =
            Diagram
              { layout =
                  SingleHand
                    { hand =
                        [ Card Spades Ace,
                          Card Spades King,
                          Card Spades Unknown,
                          Card Spades Unknown,
                          Card Spades Unknown
                        ]
                    },
                vul = Nothing,
                lead = Nothing,
                scoring = Nothing
              }

      result `shouldBe` Right expected

    it "parses a double dummy hand" do
      let result = Parser.parse "akxxx qxx jtx xx; qxx akxxx xxx kx; jx jx akxxx qxxx; xxx xxx qx axxxx"
      let expected =
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

      result `shouldBe` Right expected

    it "parses a single dummy hand" do
      let result = Parser.parse "akxxx qxx jtx xx; jx jx akxxx qxxx"
      let expected =
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

      result `shouldBe` Right expected

    it "parses a single dummy hand with vul, opening lead, and scoring" do
      let result = Parser.parse "akxxx qxx jtx xx; jx jx akxxx qxxx, d4, r/r, imps"
      let expected =
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
                lead = Just (Card Diamonds Four),
                scoring = Just Imps
              }

      result `shouldBe` Right expected

    it "parses a defensive hand from the west perspective" do
      let result = Parser.parse "akxxx qxx jtx xx < jx jx akxxx qxxx"
      let expected =
            Diagram
              { layout =
                  Defense
                    { perspective = West,
                      defender =
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
                      dummy =
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

      result `shouldBe` Right expected

    it "parses a defensive hand from the east perspective" do
      let result = Parser.parse "akxxx qxx jtx xx > jx jx akxxx qxxx"
      let expected =
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

      result `shouldBe` Right expected
