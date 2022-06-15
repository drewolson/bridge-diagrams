module Bridge.Data.Seat
  ( Seat (..),
  )
where

data Seat
  = First
  | Second
  | Third
  | Fourth
  deriving (Eq)

instance Show Seat where
  show First = "1st"
  show Second = "2nd"
  show Third = "3rd"
  show Fourth = "4th"
