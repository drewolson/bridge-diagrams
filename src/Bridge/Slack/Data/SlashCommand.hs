module Bridge.Slack.Data.SlashCommand
  ( SlashCommand (..),
  )
where

import Bridge.Slack.Json (PrefixedSnakeCaseJson (..))
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
  deriving (FromJSON) via (PrefixedSnakeCaseJson SlashCommand)
