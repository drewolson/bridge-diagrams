module Bridge.Text.Formatter
  ( codeBlock,
    format,
    helpText,
  )
where

import Bridge.Data.Card (Card)
import Bridge.Data.Card qualified as Card
import Bridge.Data.Diagram (Diagram (..))
import Bridge.Data.Hand (Hand)
import Bridge.Data.Layout (Layout (..))
import Bridge.Data.Perspective (Perspective (..))
import Bridge.Data.Scoring (Scoring (..))
import Bridge.Data.Suit (Suit (..))
import Bridge.Data.Vul (Vul (..))
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

vulLine :: Maybe Vul -> Doc Text
vulLine = \case
  Nothing -> literal "Vul: N/A"
  Just vul -> literal $ "Vul: " <> pack (show vul)

scoringLine :: Maybe Scoring -> Doc Text
scoringLine = \case
  Nothing -> empty
  Just scoring -> literal $ pack $ show scoring

infoBlock :: Maybe Vul -> Maybe Scoring -> Doc Text
infoBlock Nothing Nothing = emptyBlock
infoBlock vul scoring =
  block $
    vcat
      [ vulLine vul,
        scoringLine scoring
      ]

leadBlock :: Maybe Card -> Doc Text
leadBlock = \case
  Just card -> block $ literal $ "Lead: " <> pack (show card)
  Nothing -> emptyBlock

diagramDocument :: Diagram -> Doc Text
diagramDocument = \case
  Diagram {layout = DoubleDummy {north, south, east, west}, vul, scoring, lead} ->
    vcat
      [ infoBlock vul scoring <> handBlock north,
        handBlock west <> centerCompass <> handBlock east,
        leadBlock lead <> handBlock south
      ]
  Diagram {layout = SingleDummy {north, south}, vul, scoring, lead = Nothing} ->
    vsep
      [ handBlock north <> infoBlock vul scoring,
        handBlock south
      ]
  Diagram {layout = SingleDummy {north, south}, vul, scoring, lead} ->
    vcat
      [ infoBlock vul scoring <> handBlock north,
        leadBlock lead,
        emptyBlock <> handBlock south
      ]
  Diagram {layout = Defense {perspective = East, defender, dummy}, vul, scoring, lead = Nothing} ->
    vcat
      [ handBlock dummy <> infoBlock vul scoring,
        lowerLeftCompass <> handBlock defender
      ]
  Diagram {layout = Defense {perspective = East, defender, dummy}, vul, scoring, lead} ->
    vcat
      [ infoBlock vul scoring <> handBlock dummy,
        leadBlock lead <> lowerLeftCompass <> handBlock defender
      ]
  Diagram {layout = Defense {perspective = West, defender, dummy}, vul, scoring} ->
    vcat
      [ infoBlock vul scoring <> handBlock dummy,
        handBlock defender <> lowerRightCompass
      ]
  Diagram {layout = SingleHand {hand}, vul, scoring} ->
    handBlock hand <> infoBlock vul scoring

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

You can also optionally provide a vulnerability, opening lead, and scoring
separated by commas.

Examples:

akxxx qxx jtx xx; jx jx akxxx qxxx, r/r, d4, imps


Several types of layouts are supported.



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

Or with an opening lead, vul, and scoring:

akxxx qxx jtx xx; jx jx akxxx qxxx, d4, r/r, imps

Vul: R/R   ♠AKxxx
IMPs       ♥Qxx
           ♦JTx
           ♣xx

♦4

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
