module Bridge.Data.Card
  ( Card (..),
  )
where

import Bridge.Data.Rank (Rank)
import Bridge.Data.Suit (Suit)

data Card = Card
  { suit :: Suit,
    rank :: Rank
  }
  deriving (Eq, Ord)

instance Show Card where
  show :: Card -> String
  show Card {suit, rank} = show suit <> show rank
