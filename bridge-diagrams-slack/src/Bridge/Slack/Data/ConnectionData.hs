module Bridge.Slack.Data.ConnectionData
  ( ConnectionData (..),
  )
where

import Bridge.Slack.Data.Json (SnakeCaseJson (..))
import Data.Aeson (FromJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

data ConnectionData = ConnectionData
  { connectionDataOk :: Bool,
    connectionDataUrl :: Text
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON) via (SnakeCaseJson 14 ConnectionData)
