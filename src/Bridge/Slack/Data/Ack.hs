module Bridge.Slack.Data.Ack
  ( Ack (..),
    ackFromEvent,
    errorAck,
  )
where

import Bridge.Slack.Data.AckPayload (AckPayload (..))
import Bridge.Slack.Data.Event (Event (..))
import Bridge.Slack.Json (PrefixedSnakeCaseJson (..))
import Data.Aeson (ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

data Ack = Ack
  { ackEnvelopeId :: Text,
    ackPayload :: Maybe AckPayload
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON) via (PrefixedSnakeCaseJson Ack)

ackFromEvent :: Event -> Ack
ackFromEvent Event {eventEnvelopeId} =
  Ack {ackEnvelopeId = eventEnvelopeId, ackPayload = Nothing}

errorAck :: Event -> Text -> Ack
errorAck Event {eventEnvelopeId} text =
  Ack
    { ackEnvelopeId = eventEnvelopeId,
      ackPayload =
        Just
          AckPayload
            { ackPayloadReplaceOriginal = Just True,
              ackPayloadResponseType = "ephemeral",
              ackPayloadText = text
            }
    }
