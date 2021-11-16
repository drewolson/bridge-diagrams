module Bridge.Data.Diagram
  ( Diagram (..),
  )
where

import Bridge.Data.Card (Card)
import Bridge.Data.Layout (Layout)
import Bridge.Data.Scoring (Scoring)
import Bridge.Data.Vul (Vul)

data Diagram = Diagram
  { layout :: Layout,
    vul :: Maybe Vul,
    scoring :: Maybe Scoring,
    lead :: Maybe Card
  }
  deriving (Eq, Show)
