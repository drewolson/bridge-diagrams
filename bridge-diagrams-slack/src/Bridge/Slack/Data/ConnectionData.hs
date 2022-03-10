module Bridge.Slack.Data.ConnectionData
  ( ConnectionData (..),
  )
where

import Bridge.Slack.Json (PrefixedSnakeCaseJson (..))
import Data.Aeson (FromJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

data ConnectionData = ConnectionData
  { connectionDataOk :: Bool,
    connectionDataUrl :: Text
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON) via (PrefixedSnakeCaseJson ConnectionData)
