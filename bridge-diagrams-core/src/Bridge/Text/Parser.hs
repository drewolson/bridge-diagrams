module Bridge.Text.Parser
  ( parse,
  )
where

import Bridge.Data.Card (Card (..))
import Bridge.Data.Diagram (Diagram (..))
import Bridge.Data.Diagram qualified as Diagram
import Bridge.Data.Hand (Hand)
import Bridge.Data.Hand qualified as Hand
import Bridge.Data.Layout (Layout (..))
import Bridge.Data.Layout qualified as Layout
import Bridge.Data.Perspective (Perspective (..))
import Bridge.Data.Rank (Rank (..))
import Bridge.Data.Scoring (Scoring (..))
import Bridge.Data.Suit (Suit (..))
import Bridge.Data.Vul (Vul (..))
import Control.Applicative ((<|>))
import Control.Applicative.Permutations (intercalateEffect, toPermutation, toPermutationWithDefault)
import Data.Bifunctor (first)
import Data.Text (Text, pack, strip)
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, errorBundlePretty, runParser, sepBy1, sepEndBy1, some, try, (<?>))
import Text.Megaparsec.Char (char, space, space1, string, string')

type Parser = Parsec Void Text

parseRank :: Parser Rank
parseRank =
  (Ace <$ string' "a")
    <|> (King <$ string' "k")
    <|> (Queen <$ string' "q")
    <|> (Jack <$ string' "j")
    <|> (Ten <$ choice [string' "t", string "10"])
    <|> (Nine <$ char '9')
    <|> (Eight <$ char '8')
    <|> (Seven <$ char '7')
    <|> (Six <$ char '6')
    <|> (Five <$ char '5')
    <|> (Four <$ char '4')
    <|> (Three <$ char '3')
    <|> (Two <$ char '2')
    <|> (Unknown <$ string' "x")
    <?> "Valid Card"

parseVoidSuit :: Parser [Rank]
parseVoidSuit = [] <$ (string "-" <|> string' "void")

parseSuitHolding :: Parser [Rank]
parseSuitHolding = try parseVoidSuit <|> some parseRank

parseHand :: Parser Hand
parseHand = do
  holdings <- sepEndBy1 parseSuitHolding space1

  either fail pure $ Hand.fromHoldings holdings

parseHands :: Parser [Hand]
parseHands = sepBy1 parseHand (space *> char ';' <* space)

parseDeclarer :: Parser Layout
parseDeclarer = do
  hands <- parseHands

  either fail pure $ Layout.fromHands hands

parsePerspective :: Parser Perspective
parsePerspective = (West <$ char '<') <|> (East <$ char '>')

parseDefense :: Parser Layout
parseDefense = do
  a <- parseHand <* space
  seat <- parsePerspective <* space
  b <- parseHand

  pure $ case seat of
    East -> Defense {perspective = East, defender = b, dummy = a}
    West -> Defense {perspective = West, defender = a, dummy = b}

parseLayout :: Parser Layout
parseLayout = try parseDefense <|> parseDeclarer

parseVul :: Parser Vul
parseVul =
  choice
    [ RR <$ try (vul <* char '/' <* vul),
      WR <$ try (nonVul <* char '/' <* vul),
      RW <$ try (vul <* char '/' <* nonVul),
      WW <$ nonVul <* char '/' <* nonVul
    ]
  where
    nonVul = string' "w"
    vul = string' "r"

parseSuit :: Parser Suit
parseSuit =
  choice
    [ Spades <$ string' "s",
      Hearts <$ string' "h",
      Diamonds <$ string' "d",
      Clubs <$ string' "c"
    ]

parseCard :: Parser Card
parseCard = Card <$> parseSuit <*> parseRank

parseScoring :: Parser Scoring
parseScoring =
  choice
    [ Imps <$ string' "imps",
      Mps <$ string' "mps",
      Bam <$ string' "bam"
    ]

parseDiagram :: Parser Diagram
parseDiagram = do
  (layout, vul, scoring, lead) <-
    intercalateEffect (space *> char ',' <* space) $
      (,,,) <$> toPermutation parseLayout
        <*> toPermutationWithDefault Nothing (Just <$> parseVul)
        <*> toPermutationWithDefault Nothing (Just <$> parseScoring)
        <*> toPermutationWithDefault Nothing (Just <$> parseCard)

  either fail pure $ Diagram.new layout vul scoring lead

parse :: Text -> Either Text Diagram
parse = first (pack . errorBundlePretty) . runParser parseDiagram "" . strip
