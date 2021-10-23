module Bridge.Text.Formatter
  ( codeBlock,
    format,
    helpText,
  )
where

import Bridge.Data.Card qualified as Card
import Bridge.Data.Diagram (Diagram (..))
import Bridge.Data.Hand (Hand)
import Bridge.Data.Perspective (Perspective (..))
import Bridge.Data.Suit (Suit (..))
import Data.List (sort)
import Data.Text (Text, pack)
import Text.DocLayout (Doc, chomp, empty, lblock, literal, render, vcat, vsep)
import Text.RawString.QQ (r)

block :: Doc Text -> Doc Text
block = lblock 11

emptyBlock :: Doc Text
emptyBlock = block empty

centerCompass :: Doc Text
centerCompass =
  block $
    vcat
      [ literal " ----- ",
        literal "|  N  |",
        literal "|W   E|",
        literal "|  S  |",
        literal " ----- "
      ]

lowerLeftCompass :: Doc Text
lowerLeftCompass =
  block $
    vcat
      [ literal " ----- ",
        literal "|  N  |",
        literal "|    E|",
        literal "|     |",
        literal " ----- "
      ]

lowerRightCompass :: Doc Text
lowerRightCompass =
  block $
    vcat
      [ literal " ----- ",
        literal "|  N  |",
        literal "|W    |",
        literal "|     |",
        literal " ----- "
      ]

handBlock :: Hand -> Doc Text
handBlock = block . vcat . fmap literal . handLines
  where
    handLines :: Hand -> [Text]
    handLines hand = suitLine hand <$> [Spades, Hearts, Diamonds, Clubs]

    suitLine :: Hand -> Suit -> Text
    suitLine hand suit =
      let suitRanks = Card.rank <$> filter ((== suit) . Card.suit) hand
          holding = foldMap (pack . show) $ sort suitRanks
       in pack (show suit) <> holding

diagramDocument :: Diagram -> Doc Text
diagramDocument = \case
  DoubleDummy {north, south, east, west} ->
    vcat
      [ emptyBlock <> handBlock north,
        handBlock west <> centerCompass <> handBlock east,
        emptyBlock <> handBlock south
      ]
  SingleDummy {north, south} ->
    vsep
      [ handBlock north,
        handBlock south
      ]
  Defense {perspective = East, defender, dummy} ->
    vcat
      [ handBlock dummy,
        lowerLeftCompass <> handBlock defender
      ]
  Defense {perspective = West, defender, dummy} ->
    vcat
      [ emptyBlock <> handBlock dummy,
        handBlock defender <> lowerRightCompass
      ]
  SingleHand {hand} -> handBlock hand

format :: Diagram -> Text
format = render Nothing . chomp . diagramDocument

codeBlock :: Text -> Text
codeBlock s = "```" <> s <> "```"

helpText :: Text
helpText =
  [r|Given a plaintext representation of a hand (or many hands), a diagram will be
generated.  Hands are given as lists of suits, separated by a space. Honors can
be upper or lowercase. The character 'x' or 'X' can be provided instead of a rank.
Void suits can be represented using the '-' character or the strings "void" or
"Void".

Examples:

akxxx qtx jxx xx
akqxxxxx - - kjxxx
AKQxxxxx Void void KJxxx



Several types of diagrams are supported.



Single hand:

akxxx kqx txx xx

♠AKxxx
♥KQx
♦Txx
♣xx



Single dummy, two hands separated by ';'

akxxx qxx jtx xx; jx jx akxxx qxxx

♠AKxxx
♥Qxx
♦JTx
♣xx

♠Jx
♥Jx
♦AKxxx
♣Qxxx



Double dummy, four hands separated by ';'

akxxx qxx jtx xx; qxx akxxx xxx kx; jx jx akxxx qxxx; xxx xxx qx axxxx

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



Defense, west perspective, two hands separated by '<'

xxx xxx qx axxxx < akxxx qxx jtx xx

           ♠AKxxx
           ♥Qxx
           ♦JTx
           ♣xx
♠xxx        -----
♥xxx       |  N  |
♦Qx        |W    |
♣Axxxx     |     |
            -----



Defense, east perspective, two hands separated by '>'

akxxx qxx jtx xx > qxx akxxx xxx kx

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
