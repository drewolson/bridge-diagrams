module Bridge.Slack.Data.AckPayload
  ( AckPayload (..),
  )
where

import Bridge.Slack.Json (SnakeCaseJson (..))
import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data AckPayload = AckPayload
  { ackPayloadResponseType :: Text,
    ackPayloadText :: Text,
    ackPayloadReplaceOriginal :: Maybe Bool
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON) via (SnakeCaseJson 10 AckPayload)
