module Main
  ( main,
  )
where

import Bridge.Slack.Main qualified

main :: IO ()
main = Bridge.Slack.Main.main
