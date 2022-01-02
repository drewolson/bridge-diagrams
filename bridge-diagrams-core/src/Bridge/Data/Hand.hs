module Bridge.Data.Hand
  ( Hand,
    fromHoldings,
  )
where

import Bridge.Data.Card (Card (..))
import Bridge.Data.Rank (Rank (..))
import Bridge.Data.Suit (Suit (..))
import Bridge.Data.Unknown (hasUniqueEntries, showDuplicates)
import Control.Monad (join, unless, when)

type Hand = [Card]

fromHoldings :: [[Rank]] -> Either String Hand
fromHoldings holdings = do
  unless (length holdings == 4) do
    Left "You must provide cards for all four suits"

  let hand = join $ zipWith (fmap . Card) [Spades, Hearts, Diamonds, Clubs] holdings

  when (length hand > 13) do
    Left "Hand has more than 13 cards"

  unless (hasUniqueEntries hand) do
    Left $ "Duplicate cards in hand: " <> showDuplicates hand

  pure hand
