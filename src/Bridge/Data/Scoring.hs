module Bridge.Data.Scoring
  ( Scoring (..),
  )
where

data Scoring = Imps | Mps | Bam
  deriving (Eq)

instance Show Scoring where
  show :: Scoring -> String
  show = \case
    Imps -> "IMPs"
    Mps -> "MPs"
    Bam -> "BAM"
