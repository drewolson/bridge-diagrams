module Bridge.Data.Suit
  ( Suit (..),
  )
where

data Suit
  = Spades
  | Hearts
  | Diamonds
  | Clubs
  deriving (Eq, Ord)

instance Show Suit where
  show :: Suit -> String
  show = \case
    Spades -> "♠"
    Hearts -> "♥"
    Diamonds -> "♦"
    Clubs -> "♣"
