module Main (main) where

import System.Environment (getArgs)
import System.Process
import System.IO (Handle, hSetBuffering, BufferMode(..), hGetLine)

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
    channel:script:args -> run channel script args
    _ -> error "specify channel and script. example: twitch-app <channel> twitch-echo"

run :: String -> String -> [String] -> IO ()
run channel script args = do
  proc <- createProcess (proc script args) {
      std_in = CreatePipe
    , std_out = CreatePipe
  }
  case proc of
    (Just hWriteTo, Just hReadFrom, _, _) -> do
      print $ "Script started: " ++ script
      hSetBuffering hWriteTo NoBuffering
      client <- Twitch.connect
      forkIO $ processActions script hReadFrom client
      Twitch.authenticate client nick pass
      Twitch.joinChannel client channel hWriteTo
    _ -> error "I don't think we can reach this, createProcess throws on error"

processActions :: String -> Handle -> Twitch.Client -> IO ()
processActions name handle client = forever $ do
  Twitch.Output action <- read <$> hGetLine handle
  case action of
    Twitch.SendMessage channel message -> Twitch.sendMessage client channel message
    Twitch.Log message -> putStrLn $ "<" ++ name ++ "> " ++ message

test :: IO ()
test = run "lirik" "twitch-echo" []
