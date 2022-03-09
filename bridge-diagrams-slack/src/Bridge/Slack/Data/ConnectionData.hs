module Bridge.Slack.Data.ConnectionData
  ( ConnectionData (..),
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    Options (fieldLabelModifier),
    defaultOptions,
    genericParseJSON,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Casing (quietSnake)

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
