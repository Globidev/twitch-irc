module Main (main) where

import System.Environment (getArgs)
import System.Process
import System.IO (Handle, hPrint, hSetBuffering, BufferMode(..), hGetLine)

import Control.Concurrent (forkIO)
import Control.Monad (forever)

import qualified Twitch

-- Next step is to use Twitch API to fetch this
nick = "globinette"
pass = "oauth:c6gnjdi9tmqysf4edozkim16l77605"

main :: IO ()
main = do
  args <- getArgs
  case args of
    channel:script:_ -> run channel script
    _ -> error "specify channel and script. example: twitch-app lirik twitch-hello-world"

run :: String -> String -> IO ()
run channel scriptFile = do
  proc <- createProcess (proc scriptFile []) {
      std_in = CreatePipe
    , std_out = Inherit
    , std_err = CreatePipe
  }
  case proc of
    (Just hin, _, Just herr, _) -> do
      print "Script started"
      hSetBuffering hin NoBuffering
      client <- Twitch.connect
      forkIO $ handleActions herr client
      Twitch.authenticate client nick pass
      Twitch.joinChannel client channel hin
    _ -> error "I don't think we can reach this, createProcess throws on error"

handleActions :: Handle -> Twitch.Client -> IO ()
handleActions handle client = forever $ do
  Twitch.Output action <- read <$> hGetLine handle
  case action of
    Twitch.SendMessage channel message  -> Twitch.sendMessage client channel message

test :: IO ()
test = run "lirik" "twitch-hello-world"
