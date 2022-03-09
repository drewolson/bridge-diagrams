module Bridge.Slack.Main
  ( main,
  )
where

import Bridge.IO.Buffering qualified as Buffering
import Bridge.Slack.Data.Ack (ackFromEvent, errorAck)
import Bridge.Slack.Data.ConnectionData (ConnectionData (..))
import Bridge.Slack.Data.Event (Event (..), pattern BridgeCommand, pattern BridgeHelpCommand)
import Bridge.Slack.Data.Message (Message (..))
import Bridge.Slack.Data.PostMessage (PostMessage (..), postMessageFromSlashCommand)
import Bridge.Slack.Data.SlashCommand (SlashCommand (..))
import Bridge.Text.Formatter qualified as Formatter
import Bridge.Text.Help qualified as Help
import Bridge.Text.Parser qualified as Parser
import Control.Monad (MonadPlus (mzero), forever, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Aeson (ToJSON, decode, encode)
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Client.Conduit (RequestBody (RequestBodyLBS), method, requestBody, requestHeaders)
import Network.HTTP.Simple (Request, RequestHeaders, getResponseBody, httpJSON, httpLBS, parseRequest)
import Network.Socket (PortNumber)
import Network.WebSockets qualified as WS
import System.Environment (getEnv)
import Wuss qualified

slackRequest :: MonadIO m => String -> String -> RequestHeaders -> m Request
slackRequest token url headers = do
  initReq <- liftIO $ parseRequest url

  pure
    initReq
      { method = "POST",
        requestHeaders = [("Authorization", "Bearer " <> BS.pack token)] <> headers
      }

getConnectionData :: MonadIO m => String -> m ConnectionData
getConnectionData token = do
  req <-
    slackRequest
      token
      "https://slack.com/api/apps.connections.open"
      [("Content-Type", "application/x-www-form-urlencoded")]

  getResponseBody <$> httpJSON req

fetchConnectionInfo :: MonadIO m => String -> m (String, PortNumber, String)
fetchConnectionInfo token = do
  ConnectionData {connectionDataUrl} <- getConnectionData token
  let url = fromMaybe connectionDataUrl (T.stripPrefix "wss://" connectionDataUrl)
  let (host, path) = T.breakOn "/" url
  pure (T.unpack host, 443, T.unpack path)

postMessage :: MonadIO m => String -> PostMessage -> m ()
postMessage token message = do
  initReq <-
    slackRequest
      token
      "https://slack.com/api/chat.postMessage"
      [("Content-Type", "application/json")]

  void $ httpLBS $ initReq {requestBody = RequestBodyLBS $ encode message}

ack :: MonadIO m => WS.Connection -> Event -> m ()
ack conn = liftIO . WS.sendTextData conn . encode . ackFromEvent

sendJson :: (ToJSON a, MonadIO m) => WS.Connection -> a -> m ()
sendJson conn = liftIO . WS.sendTextData conn . encode

sendClose :: MonadIO m => WS.Connection -> m ()
sendClose conn = liftIO $ WS.sendClose conn ("close" :: Text)

receive :: (WS.WebSocketsData a, MonadIO m) => WS.Connection -> m a
receive = liftIO . WS.receiveData

handleHelpCommand :: MonadIO m => WS.Connection -> Event -> m ()
handleHelpCommand conn event =
  sendJson conn $ errorAck event $ Formatter.codeBlock Help.helpText

handleBridgeCommand :: MonadIO m => String -> WS.Connection -> Event -> SlashCommand -> m ()
handleBridgeCommand token conn event slashCommand@SlashCommand {slashCommandText} = do
  case Parser.parse $ T.strip slashCommandText of
    Left err -> do
      let errorText = Formatter.codeBlock $ "Error processing input: " <> slashCommandText <> "\n\n" <> err
      sendJson conn $ errorAck event errorText
    Right diagram -> do
      ack conn event
      let responseText = Formatter.codeBlock $ Formatter.format diagram
      postMessage token $ postMessageFromSlashCommand responseText slashCommand

loop :: Monad m => MaybeT m a -> m ()
loop = void . runMaybeT . forever

handler :: String -> WS.ClientApp ()
handler token conn = loop do
  msg <- receive conn

  liftIO $ putStrLn "Incoming event:"
  liftIO $ print msg

  case decode msg of
    Just HelloMessage ->
      pure ()
    Just (EventMessage event@BridgeHelpCommand) -> do
      handleHelpCommand conn event
    Just (EventMessage event@(BridgeCommand slashCommand)) -> do
      handleBridgeCommand token conn event slashCommand
    Just (EventMessage event) -> do
      ack conn event
    _ -> do
      sendClose conn
      mzero

main :: IO ()
main = do
  Buffering.forceLineMode

  token <- getEnv "SLACK_TOKEN"
  userToken <- getEnv "SLACK_USER_TOKEN"

  forever do
    (host, port, path) <- fetchConnectionInfo token

    Wuss.runSecureClient host port path (handler userToken)
