module Bridge.Slack.Data.AckPayload
  ( AckPayload (..),
  )
where

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
