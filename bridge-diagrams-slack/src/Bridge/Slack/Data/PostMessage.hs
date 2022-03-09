module Bridge.Slack.Data.PostMessage
  ( PostMessage (..),
    postMessageFromSlashCommand,
  )
where

import Bridge.Slack.Data.SlashCommand (SlashCommand (..))
import Data.Aeson
  ( Options (fieldLabelModifier),
    ToJSON (toJSON),
    Value,
    defaultOptions,
    genericToJSON,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Casing (quietSnake)

data PostMessage = PostMessage
  { postMessageChannel :: Text,
    postMessageAsUser :: Bool,
    postMessageText :: Text,
    postMessageUsername :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON PostMessage where
  toJSON :: PostMessage -> Value
  toJSON =
    genericToJSON $
      defaultOptions
        { fieldLabelModifier = quietSnake . drop 11
        }

postMessageFromSlashCommand :: Text -> SlashCommand -> PostMessage
postMessageFromSlashCommand text SlashCommand {slashCommandUserName, slashCommandChannelId} =
  PostMessage
    { postMessageChannel = slashCommandChannelId,
      postMessageAsUser = True,
      postMessageText = text,
      postMessageUsername = slashCommandUserName
    }
