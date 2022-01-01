module Bridge.Data.Layout
  ( Layout (..),
    fromHands,
    suitCombination,
  )
where

import Bridge.Data.Card (Card (..))
import Bridge.Data.Card qualified as Card
import Bridge.Data.Hand (Hand)
import Bridge.Data.Hand qualified as Hand
import Bridge.Data.Perspective (Perspective)
import Bridge.Data.Rank (Rank (..))
import Bridge.Data.Rank qualified as Rank
import Bridge.Data.Suit (Suit)
import Bridge.Data.Suit qualified as Suit
import Control.Monad (join, unless, when)
import Data.List (nub, (\\))

data Layout
  = SingleHand {hand :: Hand}
  | SingleDummy {north :: Hand, south :: Hand}
  | DoubleDummy {north :: Hand, east :: Hand, south :: Hand, west :: Hand}
  | Defense {perspective :: Perspective, defender :: Hand, dummy :: Hand}
  | SuitCombination {top :: [Rank], bottom :: [Rank]}
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

fromThreeHands :: Hand -> Hand -> Hand -> Either String Layout
fromThreeHands north east south = do
  let existing = join [north, south, east]

  unless (length existing == 39) do
    Left "All 39 cards must be specified when providing 3 hands"

  let west = buildFourthHand existing

  pure $ DoubleDummy {north, east, south, west}

fromHands :: [Hand] -> Either String Layout
fromHands hands = do
  unless (Hand.uniqueCards $ join hands) do
    Left "Cards in deal must be unique"

  case hands of
    [north, east, south, west] -> pure $ DoubleDummy {north, east, south, west}
    [north, east, south] -> fromThreeHands north east south
    [north, south] -> pure $ SingleDummy {north, south}
    [hand] -> pure $ SingleHand hand
    _ -> Left "Deal must be 1, 2, 3, or 4 hands"

suitCombination :: [Rank] -> [Rank] -> Either String Layout
suitCombination top bottom = do
  let knownRanks = filter (not . Rank.isUnknown) $ join [top, bottom]

  when (length knownRanks /= length (nub knownRanks)) do
    Left "Suit combination ranks must be unique"

  when (length (join [top, bottom]) > 13) do
    Left "Suit combination cannot contain more than 13 cards"

  pure $ SuitCombination {top, bottom}
