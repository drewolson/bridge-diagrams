module Bridge.Slack.Data.AckPayload
  ( AckPayload (..),
  )
where

import Bridge.Slack.Json (PrefixedSnakeCaseJson (..))
import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data AckPayload = AckPayload
  { ackPayloadResponseType :: Text,
    ackPayloadText :: Text,
    ackPayloadReplaceOriginal :: Maybe Bool
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON) via (PrefixedSnakeCaseJson AckPayload)
