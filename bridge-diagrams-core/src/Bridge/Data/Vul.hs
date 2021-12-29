module Bridge.Data.Vul
  ( Vul (..),
  )
where

data Vul = RR | RW | WR | WW
  deriving (Eq)

instance Show Vul where
  show :: Vul -> String
  show = \case
    RR -> "R/R"
    RW -> "R/W"
    WR -> "W/R"
    WW -> "W/W"
