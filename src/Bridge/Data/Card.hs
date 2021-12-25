module Bridge.Data.Card
  ( Card (..),
    isUnknown,
  )
where

import Bridge.Data.Rank (Rank)
import Bridge.Data.Rank qualified as Rank
import Bridge.Data.Suit (Suit)

data Card = Card
  { suit :: Suit,
    rank :: Rank
  }
  deriving (Eq, Ord)

isUnknown :: Card -> Bool
isUnknown = Rank.isUnknown . rank

instance Show Card where
  show :: Card -> String
  show Card {suit, rank} = show suit <> show rank
