module Bridge.Text.Formatter
  ( codeBlock,
    format,
  )
where

import Bridge.Data.Card (Card)
import Bridge.Data.Card qualified as Card
import Bridge.Data.Diagram (Diagram (..))
import Bridge.Data.Hand (Hand)
import Bridge.Data.Layout (Layout (..))
import Bridge.Data.Perspective (Perspective (..))
import Bridge.Data.Rank (Rank)
import Bridge.Data.Scoring (Scoring (..))
import Bridge.Data.Seat (Seat (..))
import Bridge.Data.Suit (Suit (..))
import Bridge.Data.Vul (Vul (..))
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import Text.DocLayout (Doc, chomp, empty, lblock, literal, render, vcat, vsep)

block :: Doc Text -> Doc Text
block = lblock 11

emptyBlock :: Doc Text
emptyBlock = block empty

linesToBlock :: [Doc Text] -> Doc Text
linesToBlock = \case
  [] -> emptyBlock
  l -> block $ vcat l

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

rankLine :: [Rank] -> Doc Text
rankLine = literal . foldMap (pack . show) . sort

handBlock :: Hand -> Doc Text
handBlock = block . vcat . handLines
  where
    handLines :: Hand -> [Doc Text]
    handLines hand = suitLine hand <$> [Spades, Hearts, Diamonds, Clubs]

    suitLine :: Hand -> Suit -> Doc Text
    suitLine hand suit =
      let suitRanks = Card.rank <$> filter ((== suit) . Card.suit) hand
       in literal (pack $ show suit) <> rankLine suitRanks

vulLine :: Vul -> Doc Text
vulLine vul = literal $ "Vul:  " <> pack (show vul)

scoringLine :: Scoring -> Doc Text
scoringLine = literal . pack . show

seatLine :: Seat -> Doc Text
seatLine seat = literal $ "Seat: " <> pack (show seat)

infoBlock :: Diagram -> Doc Text
infoBlock Diagram {vul, scoring, seat} =
  linesToBlock $
    catMaybes
      [ seatLine <$> seat,
        vulLine <$> vul,
        scoringLine <$> scoring
      ]

leadBlock :: Maybe Card -> Doc Text
leadBlock = \case
  Just card -> block $ literal $ "Lead: " <> pack (show card)
  Nothing -> emptyBlock

diagramDocument :: Diagram -> Doc Text
diagramDocument = \case
  d@Diagram {layout = DoubleDummy {north, south, east, west}, lead} ->
    vcat
      [ infoBlock d <> handBlock north,
        handBlock west <> centerCompass <> handBlock east,
        leadBlock lead <> handBlock south
      ]
  d@Diagram {layout = SingleDummy {north, south}, lead = Nothing} ->
    vsep
      [ handBlock north <> infoBlock d,
        handBlock south
      ]
  d@Diagram {layout = SingleDummy {north, south}, lead} ->
    vcat
      [ infoBlock d <> handBlock north,
        leadBlock lead,
        emptyBlock <> handBlock south
      ]
  d@Diagram {layout = Defense {perspective = East, defender, dummy}, lead = Nothing} ->
    vcat
      [ handBlock dummy <> infoBlock d,
        lowerLeftCompass <> handBlock defender
      ]
  d@Diagram {layout = Defense {perspective = East, defender, dummy}, lead} ->
    vcat
      [ infoBlock d <> handBlock dummy,
        leadBlock lead <> lowerLeftCompass <> handBlock defender
      ]
  d@Diagram {layout = Defense {perspective = West, defender, dummy}} ->
    vcat
      [ infoBlock d <> handBlock dummy,
        handBlock defender <> lowerRightCompass
      ]
  d@Diagram {layout = SingleHand {hand}} ->
    handBlock hand <> infoBlock d

format :: Diagram -> Text
format = render Nothing . chomp . diagramDocument

codeBlock :: Text -> Text
codeBlock s = "```" <> s <> "```"
