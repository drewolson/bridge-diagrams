module Bridge.Data.Layout
  ( Layout (..),
    fromHands,
  )
where

import Bridge.Data.Card (Card (..))
import Bridge.Data.Card qualified as Card
import Bridge.Data.Hand (Hand)
import Bridge.Data.Perspective (Perspective)
import Bridge.Data.Rank (Rank (..))
import Bridge.Data.Suit (Suit)
import Bridge.Data.Suit qualified as Suit
import Bridge.Data.Unknown qualified as Unknown
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
      | Unknown.areAllKnown existing = Card.enumerateSuit suit \\ existing
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
  unless (Unknown.hasUniqueEntries $ join hands) do
    Left $ "Duplicate cards in deal: " <> Unknown.showDuplicates (join hands)

  case hands of
    [north, east, south, west] -> pure $ DoubleDummy {north, east, south, west}
    [north, east, south] -> fromThreeHands north east south
    [north, south] -> pure $ SingleDummy {north, south}
    [hand] -> pure $ SingleHand hand
    _ -> Left "Deal must be 1, 2, 3, or 4 hands"
