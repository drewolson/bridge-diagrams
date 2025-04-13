module Bridge.Text.ParserSpec
  ( spec,
  )
where

import Bridge.Data.Card (Card (..))
import Bridge.Data.Diagram (Diagram (..))
import Bridge.Data.Layout (Layout (..))
import Bridge.Data.Perspective (Perspective (..))
import Bridge.Data.Rank (Rank (..))
import Bridge.Data.Scoring (Scoring (..))
import Bridge.Data.Seat (Seat (..))
import Bridge.Data.Suit (Suit (..))
import Bridge.Data.Vul (Vul (..))
import Bridge.Text.Parser qualified as Parser
import Data.Either (fromLeft)
import Data.Either qualified as Either
import Data.Text qualified as Text
import Test.Hspec (Spec, describe, it, parallel, shouldBe, shouldSatisfy)

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
                scoring = Nothing,
                seat = Nothing
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
                scoring = Nothing,
                seat = Nothing
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
                scoring = Nothing,
                seat = Nothing
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
                scoring = Nothing,
                seat = Nothing
              }

      result `shouldBe` Right expected

    it "parses a double dummy hand with only three hands specified" do
      let result = Parser.parse "akxxx qxx jtx xx; qxx akxxx xxx kx; jx jx akxxx qxxx"
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
                        [ Card Spades Ten,
                          Card Spades Unknown,
                          Card Spades Unknown,
                          Card Hearts Ten,
                          Card Hearts Unknown,
                          Card Hearts Unknown,
                          Card Diamonds Queen,
                          Card Diamonds Unknown,
                          Card Clubs Ace,
                          Card Clubs Jack,
                          Card Clubs Ten,
                          Card Clubs Unknown,
                          Card Clubs Unknown
                        ]
                    },
                vul = Nothing,
                lead = Nothing,
                scoring = Nothing,
                seat = Nothing
              }

      result `shouldBe` Right expected

    it "does not parse a double dummy hand with only three hands specified if less than 39 cards are specified" do
      let result = Parser.parse "akx - - -; qxx - - -; jx - - -"
      let err = fromLeft "" result

      err `shouldSatisfy` Text.isInfixOf "All 39 cards must be specified when providing 3 hands"

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
                scoring = Nothing,
                seat = Nothing
              }

      result `shouldBe` Right expected

    it "parses a single dummy hand with seat, vul, opening lead, and scoring" do
      let result = Parser.parse "1st, akxxx qxx jtx xx; jx jx akxxx qxxx, d4, r/w, imps"
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
                vul = Just RW,
                lead = Just (Card Diamonds Four),
                scoring = Just Imps,
                seat = Just First
              }

      result `shouldBe` Right expected

    it "does not allow non-unique opening leads" do
      let err = Either.fromLeft "boom" $ Parser.parse "akxxx qxx jtx xx; jx jx akxxx qxxx, sa"

      err `shouldSatisfy` Text.isInfixOf "Opening lead present in another hand"

    it "does not allow unknown spots as opening leads" do
      let err = Either.fromLeft "boom" $ Parser.parse "akxxx qxx jtx xx; jx jx akxxx qxxx, sx"

      err `shouldSatisfy` Text.isInfixOf "Opening lead cannot be an unknown spot card"

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
                scoring = Nothing,
                seat = Nothing
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
                scoring = Nothing,
                seat = Nothing
              }

      result `shouldBe` Right expected
