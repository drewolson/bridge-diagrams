module Bridge.Slack.Data.Event
  ( Event (..),
    pattern BridgeHelpCommand,
    pattern BridgeCommand,
  )
where

import Bridge.Slack.Data.EventPayload (EventPayload (..))
import Bridge.Slack.Data.SlashCommand (SlashCommand (..))
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Text (Text)

data Event = Event
  { eventEnvelopeId :: Text,
    eventAcceptsResponsePayload :: Bool,
    eventPayload :: EventPayload
  }
  deriving (Eq, Show)

instance FromJSON Event where
  parseJSON = withObject "EventMessage" \obj -> do
    messageType :: Text <- obj .: "type"
    eventEnvelopeId <- obj .: "envelope_id"
    eventAcceptsResponsePayload <- obj .: "accepts_response_payload"
    eventPayload <-
      case messageType of
        "slash_commands" -> SlashCommandPayload <$> obj .: "payload"
        _ -> pure UnknownPayload

    pure $ Event {eventEnvelopeId, eventAcceptsResponsePayload, eventPayload}

pattern BridgeHelpCommand :: Event
pattern BridgeHelpCommand <-
  Event
    { eventPayload =
        SlashCommandPayload
          SlashCommand
            { slashCommandCommand = "/bridge",
              slashCommandText = "help"
            }
    }

pattern BridgeCommand :: SlashCommand -> Event
pattern BridgeCommand slashCommand <-
  Event
    { eventPayload =
        SlashCommandPayload
          slashCommand@SlashCommand
            { slashCommandCommand = "/bridge"
            }
    }
