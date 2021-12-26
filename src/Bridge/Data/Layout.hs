module Bridge.Data.Layout
  ( Layout (..),
    fromHands,
  )
where

import Bridge.Data.Card (Card (..))
import Bridge.Data.Card qualified as Card
import Bridge.Data.Hand (Hand)
import Bridge.Data.Hand qualified as Hand
import Bridge.Data.Perspective (Perspective)
import Bridge.Data.Rank (Rank (..))
import Bridge.Data.Suit (Suit)
import Bridge.Data.Suit qualified as Suit
import Control.Monad (join, unless)
import Data.List (sort, (\\))

data Layout
  = SingleHand {hand :: Hand}
  | SingleDummy {north :: Hand, south :: Hand}
  | DoubleDummy {north :: Hand, east :: Hand, south :: Hand, west :: Hand}
  | Defense {perspective :: Perspective, defender :: Hand, dummy :: Hand}
  deriving (Eq, Show)

missingSpots :: [Card] -> [Card]
missingSpots cards = foldMap suitSpots Suit.enumerate
  where
    suitSpots :: Suit -> [Card]
    suitSpots suit = replicate (suitCount suit) (Card suit Unknown)

    suitCount :: Suit -> Int
    suitCount suit = 13 - length (filter ((== suit) . Card.suit) cards)

buildFourthHand :: [Card] -> Hand
buildFourthHand cards
  | not $ any Card.isUnknown cards = Card.enumerate \\ cards
  | otherwise =
    let honors = Card.honors \\ filter Card.isHonor cards
     in sort $ honors ++ missingSpots (cards ++ honors)

fromHands :: [Hand] -> Either String Layout
fromHands hands = do
  unless (Hand.uniqueCards $ join hands) do
    Left "Cards in deal must be unique"

  case hands of
    [north, east, south, west] -> pure $ DoubleDummy {north, east, south, west}
    [north, east, south] ->
      let west = buildFourthHand $ join [north, east, south]
       in pure $ DoubleDummy {north, east, south, west}
    [north, south] -> pure $ SingleDummy {north, south}
    [hand] -> pure $ SingleHand hand
    _ -> Left "Deal must be 1, 2, 3, or 4 hands"
