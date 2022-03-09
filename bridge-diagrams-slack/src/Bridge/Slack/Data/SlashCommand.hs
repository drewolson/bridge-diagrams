module Bridge.Slack.Data.SlashCommand
  ( SlashCommand (..),
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
