module Bridge.Cli.Main
  ( main,
  )
where

import Bridge.Text.Formatter qualified as Formatter
import Bridge.Text.Parser qualified as Parser
import Data.Text.IO qualified as Text.IO

main :: IO ()
main = do
  input <- Text.IO.getContents

  case Parser.parse input of
    Left e -> do
      Text.IO.putStrLn $ "Error: " <> e
    Right deal ->
      Text.IO.putStrLn $ Formatter.format deal
