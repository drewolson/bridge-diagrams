module Bridge.Data.Diagram
  ( Diagram (..),
    new,
  )
where

import Bridge.Data.Card (Card)
import Bridge.Data.Card qualified as Card
import Bridge.Data.Hand qualified as Hand
import Bridge.Data.Layout (Layout (..))
import Bridge.Data.Perspective (Perspective (..))
import Bridge.Data.Scoring (Scoring)
import Bridge.Data.Vul (Vul)
import Control.Monad (unless, when)

data Diagram = Diagram
  { layout :: Layout,
    vul :: Maybe Vul,
    scoring :: Maybe Scoring,
    lead :: Maybe Card
  }
  deriving (Eq, Show)

isUniqueLead :: Layout -> Maybe Card -> Bool
isUniqueLead _ Nothing = True
isUniqueLead Defense {perspective = West} _ = True
isUniqueLead Defense {perspective = East, defender, dummy} (Just lead) =
  lead `notElem` Hand.knownCards (defender ++ dummy)
isUniqueLead SingleDummy {north, south} (Just lead) =
  lead `notElem` Hand.knownCards (north ++ south)
isUniqueLead DoubleDummy {west} (Just lead) =
  lead `elem` Hand.knownCards west
isUniqueLead _ _ = True

isUnknownLead :: Maybe Card -> Bool
isUnknownLead Nothing = False
isUnknownLead (Just card) = Card.isUnknown card

validateLead :: Layout -> Maybe Card -> Either String ()
validateLead layout lead = do
  unless (isUniqueLead layout lead) do
    Left "Opening lead present in another hand"

  when (isUnknownLead lead) do
    Left "Opening lead cannot be an unknown spot card"

new :: Layout -> Maybe Vul -> Maybe Scoring -> Maybe Card -> Either String Diagram
new layout vul scoring lead = do
  validateLead layout lead

  pure $ Diagram {layout, vul, scoring, lead}
