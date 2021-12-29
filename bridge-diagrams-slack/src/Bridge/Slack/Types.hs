module Bridge.Slack.Types
  ( ConnectionData (..),
    Event (..),
    Message (..),
    PostMessage (..),
    SlashCommand (..),
    pattern BridgeCommand,
    pattern BridgeHelpCommand,
    ackFromEvent,
    errorAck,
    postMessageFromSlashCommand,
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    Options (fieldLabelModifier, omitNothingFields),
    ToJSON (toJSON),
    Value (Object),
    defaultOptions,
    genericParseJSON,
    genericToJSON,
    withObject,
    (.:),
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Casing (quietSnake)

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

data ConnectionData = ConnectionData
  { connectionDataOk :: Bool,
    connectionDataUrl :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON ConnectionData where
  parseJSON =
    genericParseJSON $
      defaultOptions
        { fieldLabelModifier = quietSnake . drop 14
        }

data SlashCommand = SlashCommand
  { slashCommandCommand :: Text,
    slashCommandText :: Text,
    slashCommandUserName :: Text,
    slashCommandChannelId :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON SlashCommand where
  parseJSON =
    genericParseJSON $
      defaultOptions
        { fieldLabelModifier = quietSnake . drop 12
        }

data EventPayload
  = SlashCommandPayload SlashCommand
  | UnknownPayload
  deriving (Eq, Show)

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

data Message
  = HelloMessage
  | DisconnectMessage
  | EventMessage Event
  deriving (Eq, Show)

instance FromJSON Message where
  parseJSON = withObject "Message" \obj -> do
    messageType :: Text <- obj .: "type"

    case messageType of
      "hello" -> pure HelloMessage
      "disconnect" -> pure DisconnectMessage
      _ -> EventMessage <$> parseJSON (Object obj)

data PostMessage = PostMessage
  { postMessageChannel :: Text,
    postMessageAsUser :: Bool,
    postMessageText :: Text,
    postMessageUsername :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON PostMessage where
  toJSON :: PostMessage -> Value
  toJSON =
    genericToJSON $
      defaultOptions
        { fieldLabelModifier = quietSnake . drop 11
        }

postMessageFromSlashCommand :: Text -> SlashCommand -> PostMessage
postMessageFromSlashCommand text SlashCommand {slashCommandUserName, slashCommandChannelId} =
  PostMessage
    { postMessageChannel = slashCommandChannelId,
      postMessageAsUser = True,
      postMessageText = text,
      postMessageUsername = slashCommandUserName
    }

data AckPayload = AckPayload
  { ackPayloadResponseType :: Text,
    ackPayloadText :: Text,
    ackPayloadReplaceOriginal :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON AckPayload where
  toJSON :: AckPayload -> Value
  toJSON =
    genericToJSON $
      defaultOptions
        { omitNothingFields = True,
          fieldLabelModifier = quietSnake . drop 10
        }

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
