module Bridge.Slack.Data.Ack
  ( Ack (..),
    ackFromEvent,
    errorAck,
  )
where

import Bridge.Slack.Data.AckPayload (AckPayload (..))
import Bridge.Slack.Data.Event (Event (..))
import Data.Aeson
  ( Options (fieldLabelModifier, omitNothingFields),
    ToJSON (toJSON),
    Value,
    defaultOptions,
    genericToJSON,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Casing (quietSnake)

data Ack = Ack
  { ackEnvelopeId :: Text,
    ackPayload :: Maybe AckPayload
  }
  deriving (Eq, Show, Generic)

instance ToJSON Ack where
  toJSON :: Ack -> Value
  toJSON =
    genericToJSON $
      defaultOptions
        { omitNothingFields = True,
          fieldLabelModifier = quietSnake . drop 3
        }

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
