module Bridge.Data.Card
  ( Card (..),
    enumerate,
    enumerateSuit,
    honors,
    suitHonors,
    isHonor,
    isUnknown,
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

instance Show Card where
  show :: Card -> String
  show Card {suit, rank} = show suit <> show rank

isUnknown :: Card -> Bool
isUnknown = Rank.isUnknown . rank

isHonor :: Card -> Bool
isHonor = Rank.isHonor . rank

enumerate :: [Card]
enumerate = do
  suit <- Suit.enumerate
  rank <- Rank.enumerate

  pure $ Card {suit, rank}

enumerateSuit :: Suit -> [Card]
enumerateSuit s = filter ((== s) . suit) enumerate

honors :: [Card]
honors = do
  suit <- Suit.enumerate
  rank <- Rank.honors

  pure $ Card {suit, rank}

suitHonors :: Suit -> [Card]
suitHonors s = filter ((== s) . suit) honors
