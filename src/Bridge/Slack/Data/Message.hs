module Bridge.Slack.Data.Message
  ( Message (..),
  )
where

import Bridge.Slack.Data.Event (Event (..))
import Data.Aeson (FromJSON (parseJSON), Value (Object), withObject, (.:))
import Data.Text (Text)

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
