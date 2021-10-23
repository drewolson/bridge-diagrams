module Bridge.IO.Buffering
  ( forceLineMode,
  )
where

import System.IO (BufferMode (LineBuffering), hSetBuffering, stdin, stdout)

forceLineMode :: IO ()
forceLineMode = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
