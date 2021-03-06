module Bridge.Data.Rank
  ( Rank (..),
    enumerate,
    honors,
    isHonor,
  )
where

import Bridge.Data.Unknown (Unknown (..))

data Rank
  = Ace
  | King
  | Queen
  | Jack
  | Ten
  | Nine
  | Eight
  | Seven
  | Six
  | Five
  | Four
  | Three
  | Two
  | Unknown
  deriving (Eq, Ord, Bounded, Enum)

instance Show Rank where
  show :: Rank -> String
  show = \case
    Ace -> "A"
    King -> "K"
    Queen -> "Q"
    Jack -> "J"
    Ten -> "T"
    Nine -> "9"
    Eight -> "8"
    Seven -> "7"
    Six -> "6"
    Five -> "5"
    Four -> "4"
    Three -> "3"
    Two -> "2"
    Unknown -> "x"

instance Unknown Rank where
  isUnknown :: Rank -> Bool
  isUnknown = (== Unknown)

enumerate :: [Rank]
enumerate = [Ace .. Two]

honors :: [Rank]
honors = [Ace .. Ten]

isHonor :: Rank -> Bool
isHonor = (`elem` honors)
