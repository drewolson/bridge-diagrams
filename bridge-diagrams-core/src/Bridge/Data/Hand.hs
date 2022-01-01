module Bridge.Data.Hand
  ( Hand,
    duplicatedCards,
    fromHoldings,
    knownCards,
    uniqueCards,
    showDuplicates,
  )
where

import Bridge.Data.Card (Card (..))
import Bridge.Data.Card qualified as Card
import Bridge.Data.Rank (Rank (..))
import Bridge.Data.Suit (Suit (..))
import Control.Monad (join, unless, when)
import Data.List (intercalate, nub, (\\))

type Hand = [Card]

knownCards :: Hand -> Hand
knownCards = filter (not . Card.isUnknown)

uniqueCards :: Hand -> Bool
uniqueCards hand =
  let known = knownCards hand
   in known == nub known

duplicatedCards :: Hand -> [Card]
duplicatedCards hand =
  let known = knownCards hand
   in known \\ nub known

showDuplicates :: Hand -> String
showDuplicates = intercalate "," . fmap show . duplicatedCards

fromHoldings :: [[Rank]] -> Either String Hand
fromHoldings holdings = do
  unless (length holdings == 4) do
    Left "You must provide cards for all four suits"

  let hand = join $ zipWith (fmap . Card) [Spades, Hearts, Diamonds, Clubs] holdings

  when (length hand > 13) do
    Left "Hand has more than 13 cards"

  unless (uniqueCards hand) do
    Left $ "Duplicate cards in hand: " <> showDuplicates hand

  pure hand
