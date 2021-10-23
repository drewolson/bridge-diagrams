module Bridge.Text.Parser
  ( parse,
  )
where

import Bridge.Data.Card (Card (..))
import Bridge.Data.Diagram (Diagram (..))
import Bridge.Data.Hand (Hand)
import Bridge.Data.Perspective (Perspective (..))
import Bridge.Data.Rank (Rank (..))
import Bridge.Data.Suit (Suit (..))
import Control.Applicative ((<|>))
import Control.Monad (join, unless, when)
import Data.Bifunctor (first)
import Data.List (nub)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, errorBundlePretty, runParser, sepBy1, sepEndBy1, some, try, (<?>))
import Text.Megaparsec.Char (char, space, space1, string)

type Parser = Parsec Void Text

knownCards :: [Card] -> [Card]
knownCards = filter ((/= Unknown) . rank)

uniqueCards :: [Card] -> Bool
uniqueCards cards =
  let known = knownCards cards
   in known == nub known

parseRank :: Parser Rank
parseRank =
  (Ace <$ choice [char 'A', char 'a'])
    <|> (King <$ choice [char 'K', char 'k'])
    <|> (Queen <$ choice [char 'Q', char 'q'])
    <|> (Jack <$ choice [char 'J', char 'j'])
    <|> (Ten <$ choice [try $ string "T", try $ string "t", try $ string "10"])
    <|> (Nine <$ char '9')
    <|> (Eight <$ char '8')
    <|> (Seven <$ char '7')
    <|> (Six <$ char '6')
    <|> (Five <$ char '5')
    <|> (Four <$ char '4')
    <|> (Three <$ char '3')
    <|> (Two <$ char '2')
    <|> (Unknown <$ choice [char 'x', char 'X'])
    <?> "Valid Card"

parseVoidSuit :: Parser [Rank]
parseVoidSuit = [] <$ (string "-" <|> string "void" <|> string "Void")

parseSuitHolding :: Parser [Rank]
parseSuitHolding = try parseVoidSuit <|> some parseRank

parseHand :: Parser Hand
parseHand = do
  holdings <- sepEndBy1 parseSuitHolding space1

  unless (length holdings == 4) do
    fail "You must provide cards for all four suits"

  let hand = join $ zipWith (fmap . Card) [Spades, Hearts, Diamonds, Clubs] holdings

  when (length hand > 13) do
    fail "Hand has more than 13 cards"

  unless (uniqueCards hand) do
    fail "Cards in hand must be unique"

  pure hand

parseDeclarer :: Parser Diagram
parseDeclarer = do
  hands <- sepBy1 parseHand (space *> char ';' <* space)

  unless (uniqueCards $ join hands) do
    fail "Cards in deal must be unique"

  case hands of
    [north, east, south, west] -> pure $ DoubleDummy {north, east, south, west}
    [north, south] -> pure $ SingleDummy {north, south}
    [hand] -> pure $ SingleHand hand
    _ -> fail "Deal must be 1, 2, or 4 hands"

parsePerspective :: Parser Perspective
parsePerspective = (West <$ char '<') <|> (East <$ char '>')

parseDefense :: Parser Diagram
parseDefense = do
  a <- parseHand <* space
  seat <- parsePerspective <* space
  b <- parseHand

  pure $ case seat of
    East -> Defense {perspective = East, defender = b, dummy = a}
    West -> Defense {perspective = West, defender = a, dummy = b}

parseDiagram :: Parser Diagram
parseDiagram = try parseDefense <|> parseDeclarer

parse :: Text -> Either Text Diagram
parse = first (pack . errorBundlePretty) . runParser parseDiagram ""
