module Bridge.Data.Layout
  ( Layout (..),
    fromHands,
  )
where

import Bridge.Data.Hand (Hand)
import Bridge.Data.Hand qualified as Hand
import Bridge.Data.Perspective (Perspective)
import Control.Monad (join, unless)

data Layout
  = SingleHand {hand :: Hand}
  | SingleDummy {north :: Hand, south :: Hand}
  | DoubleDummy {north :: Hand, east :: Hand, south :: Hand, west :: Hand}
  | Defense {perspective :: Perspective, defender :: Hand, dummy :: Hand}
  deriving (Eq, Show)

fromHands :: [Hand] -> Either String Layout
fromHands hands = do
  unless (Hand.uniqueCards $ join hands) do
    Left "Cards in deal must be unique"

  case hands of
    [north, east, south, west] -> pure $ DoubleDummy {north, east, south, west}
    [north, south] -> pure $ SingleDummy {north, south}
    [hand] -> pure $ SingleHand hand
    _ -> Left "Deal must be 1, 2, or 4 hands"
