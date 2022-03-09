module Bridge.Slack.Data.SlashCommand
  ( SlashCommand (..),
  )
where

import Bridge.Slack.Data.Json (SnakeCaseJson (..))
import Data.Aeson (FromJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

data SlashCommand = SlashCommand
  { slashCommandCommand :: Text,
    slashCommandText :: Text,
    slashCommandUserName :: Text,
    slashCommandChannelId :: Text
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON) via (SnakeCaseJson 12 SlashCommand)
