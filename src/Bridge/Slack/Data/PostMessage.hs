module Bridge.Slack.Data.PostMessage
  ( PostMessage (..),
    postMessageFromSlashCommand,
  )
where

import Bridge.Slack.Data.SlashCommand (SlashCommand (..))
import Bridge.Slack.Json (PrefixedSnakeCaseJson (..))
import Data.Aeson (ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

data PostMessage = PostMessage
  { postMessageChannel :: Text,
    postMessageAsUser :: Bool,
    postMessageText :: Text,
    postMessageUsername :: Text
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON) via (PrefixedSnakeCaseJson PostMessage)

postMessageFromSlashCommand :: Text -> SlashCommand -> PostMessage
postMessageFromSlashCommand text SlashCommand {slashCommandUserName, slashCommandChannelId} =
  PostMessage
    { postMessageChannel = slashCommandChannelId,
      postMessageAsUser = True,
      postMessageText = text,
      postMessageUsername = slashCommandUserName
    }
