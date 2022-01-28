module Bridge.Discord.Main
  ( main,
  )
where

import Bridge.IO.Buffering qualified as Buffering
import Bridge.Text.Formatter qualified as Formatter
import Bridge.Text.Help qualified as Help
import Bridge.Text.Parser qualified as Parser
import Control.Monad (void)
import Data.Either.Combinators (mapBoth)
import Data.Foldable (traverse_)
import Data.Text (Text, pack, strip, stripPrefix)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Discord
  ( DiscordHandler,
    def,
    discordOnEvent,
    discordOnLog,
    discordToken,
    restCall,
    runDiscord,
  )
import Discord.Requests
  ( ChannelRequest (CreateMessage, DeleteMessage),
    UserRequest (CreateDM),
  )
import Discord.Types
  ( Channel (channelId),
    Event (MessageCreate),
    Message (messageAuthor, messageChannelId, messageContent, messageId),
    User (userId, userIsBot, userName),
  )
import System.Environment (getEnv)

pattern BridgeHelp :: Message
pattern BridgeHelp <- (strip . messageContent -> "!bridge help")

pattern BridgeCommand :: Text -> Message
pattern BridgeCommand a <- (stripPrefix "!bridge" . strip . messageContent -> Just a)

deleteMessage :: Message -> DiscordHandler ()
deleteMessage m =
  void $ restCall $ DeleteMessage (messageChannelId m, messageId m)

withDm :: Message -> (Channel -> DiscordHandler ()) -> DiscordHandler ()
withDm m cb = do
  result <- restCall $ CreateDM (userId $ messageAuthor m)

  case result of
    Left _ -> pure ()
    Right channel -> cb channel

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

processInput :: Text -> Either Text Text
processInput =
  mapBoth Formatter.codeBlock Formatter.codeBlock
    . fmap Formatter.format
    . Parser.parse

splitToSize :: Text -> [Text]
splitToSize = go [] . Text.lines
  where
    maxMessageSize :: Int
    maxMessageSize = 2000

    go :: [[Text]] -> [Text] -> [Text]
    go [] [] = []
    go acc [] = reverse $ fmap (Text.unlines . reverse) acc
    go [] (h : t) = go [[h]] t
    go acc@(curr : rest) (h : t) =
      let new = h : curr
          len = sum (fmap Text.length new) + length new - 1
       in if len < maxMessageSize
            then go (new : rest) t
            else go ([h] : acc) t

helpHandler :: Message -> DiscordHandler ()
helpHandler m = do
  deleteMessage m

  withDm m \channel -> do
    let messages = Formatter.codeBlock <$> splitToSize Help.helpText
    traverse_ (restCall . CreateMessage (channelId channel)) messages

commandHandler :: Message -> Text -> DiscordHandler ()
commandHandler m input = do
  deleteMessage m

  case processInput input of
    Left err ->
      withDm m \channel -> do
        let prefix = Formatter.codeBlock $ "Error processing message: " <> input
        void $ restCall $ CreateMessage (channelId channel) (prefix <> err)
    Right diagram -> do
      let name = userName $ messageAuthor m
      let response = "@" <> name <> ":\n" <> diagram
      void $ restCall $ CreateMessage (messageChannelId m) response

eventHandler :: Event -> DiscordHandler ()
eventHandler = \case
  MessageCreate m | fromBot m -> pure ()
  MessageCreate m@BridgeHelp -> helpHandler m
  MessageCreate m@(BridgeCommand input) -> commandHandler m input
  _ -> pure ()

main :: IO ()
main = do
  Buffering.forceLineMode

  token <- pack <$> getEnv "DISCORD_TOKEN"
  err <-
    runDiscord $
      def
        { discordToken = token,
          discordOnEvent = eventHandler,
          discordOnLog = Text.IO.putStrLn
        }

  Text.IO.putStrLn err
