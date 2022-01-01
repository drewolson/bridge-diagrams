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
import Bridge.Data.Suit (Suit (..))
import Bridge.Data.Vul (Vul (..))
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import Data.Text qualified as Text
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
vulLine vul = literal $ "Vul: " <> pack (show vul)

scoringLine :: Scoring -> Doc Text
scoringLine = literal . pack . show

infoBlock :: Maybe Vul -> Maybe Scoring -> Doc Text
infoBlock vul scoring =
  linesToBlock $
    catMaybes
      [ vulLine <$> vul,
        scoringLine <$> scoring
      ]

leadBlock :: Maybe Card -> Doc Text
leadBlock = \case
  Just card -> block $ literal $ "Lead: " <> pack (show card)
  Nothing -> emptyBlock

comboBlock :: [Rank] -> [Rank] -> Doc Text
comboBlock top bottom =
  let width = maximum $ fmap length [top, bottom]
   in block $
        vcat
          [ rankLine top,
            literal $ Text.replicate width "-",
            rankLine bottom
          ]

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
  Diagram {layout = SuitCombination {top, bottom}, vul, scoring} ->
    comboBlock top bottom <> infoBlock vul scoring

format :: Diagram -> Text
format = render Nothing . chomp . diagramDocument

codeBlock :: Text -> Text
codeBlock s = "```" <> s <> "```"
