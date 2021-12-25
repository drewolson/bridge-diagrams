module Bridge.Data.Card
  ( Card (..),
    isUnknown,
    enumerate,
    honors,
  )
where

import Bridge.Data.Rank (Rank)
import Bridge.Data.Rank qualified as Rank
import Bridge.Data.Suit (Suit)
import Bridge.Data.Suit qualified as Suit

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

enumerate :: [Card]
enumerate = do
  suit <- Suit.enumerate
  rank <- Rank.enumerate

  pure $ Card {suit, rank}

honors :: [Card]
honors = do
  suit <- Suit.enumerate
  rank <- Rank.honors

  pure $ Card {suit, rank}
