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
import Data.List ((\\))

data Layout
  = SingleHand {hand :: Hand}
  | SingleDummy {north :: Hand, south :: Hand}
  | DoubleDummy {north :: Hand, east :: Hand, south :: Hand, west :: Hand}
  | Defense {perspective :: Perspective, defender :: Hand, dummy :: Hand}
  deriving (Eq, Show)

buildFourthHand :: [Card] -> Hand
buildFourthHand cards = foldMap (missingSuitCards . ofSuit) Suit.enumerate
  where
    ofSuit :: Suit -> (Suit, [Card])
    ofSuit suit = (suit, filter ((== suit) . Card.suit) cards)

    missingSuitCards :: (Suit, [Card]) -> [Card]
    missingSuitCards (suit, existing)
      | not $ any Card.isUnknown existing = Card.enumerateSuit suit \\ existing
      | otherwise =
        let honors = Card.suitHonors suit \\ filter Card.isHonor existing
            hand = honors ++ repeat (Card suit Unknown)
         in take (13 - length existing) hand

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
