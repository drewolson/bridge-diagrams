module Bridge.Slack.Data.EventPayload
  ( EventPayload (..),
  )
where

import Bridge.Slack.Data.SlashCommand (SlashCommand (..))

data EventPayload
  = SlashCommandPayload SlashCommand
  | UnknownPayload
  deriving (Eq, Show)
