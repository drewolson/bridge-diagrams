module Bridge.Discord.Formatter
  ( formatError,
    formatDiagram,
    formattedHelpText,
  )
where

import Bridge.Data.Diagram (Diagram)
import Bridge.Data.Diagram qualified as Diagram
import Bridge.Text.Formatter qualified as Formatter
import Bridge.Text.Help qualified as Help
import Data.Text (Text)
import Data.Text qualified as Text

formatError :: Text -> Text -> Text
formatError input err =
  let body = "Error processing message: " <> input <> "\n\n" <> err
   in Formatter.codeBlock body

formatDiagram :: Diagram -> Text
formatDiagram diagram =
  let output = Formatter.codeBlock $ Formatter.format diagram
   in if Diagram.spoiler diagram
        then "||" <> output <> "||"
        else output

formattedHelpText :: [Text]
formattedHelpText = Formatter.codeBlock <$> splitToSize Help.helpText

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
