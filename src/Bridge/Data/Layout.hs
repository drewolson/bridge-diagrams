module Bridge.Data.Layout
  ( Layout (..),
  )
where

import Bridge.Data.Hand (Hand)
import Bridge.Data.Perspective (Perspective)

data Layout
  = SingleHand {hand :: Hand}
  | SingleDummy {north :: Hand, south :: Hand}
  | DoubleDummy {north :: Hand, east :: Hand, south :: Hand, west :: Hand}
  | Defense {perspective :: Perspective, defender :: Hand, dummy :: Hand}
  deriving (Eq, Show)
