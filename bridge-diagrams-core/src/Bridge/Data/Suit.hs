module Bridge.Data.Suit
  ( Suit (..),
    enumerate,
  )
where

data Suit
  = Spades
  | Hearts
  | Diamonds
  | Clubs
  deriving (Eq, Ord, Bounded, Enum)

instance Show Suit where
  show :: Suit -> String
  show = \case
    Spades -> "♠"
    Hearts -> "♥"
    Diamonds -> "♦"
    Clubs -> "♣"

enumerate :: [Suit]
enumerate = [minBound .. maxBound]
