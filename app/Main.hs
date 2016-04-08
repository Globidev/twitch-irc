module Main (main) where

import System.Environment (getArgs)
import System.Process
import System.IO (Handle, hSetBuffering, BufferMode(..), hGetLine, stdout)

import Control.Concurrent (forkIO)
import Control.Monad (forever)

import Data.Monoid (mappend)
import Data.Text.IO (hPutStrLn)

import qualified Data.Text as T

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
  (Just hWriteTo, Just hReadFrom, _, _) <- createProcess (proc script args)
                                           { std_in = CreatePipe
                                           , std_out = CreatePipe
                                           }
  print $ "Script started: " ++ script
  hSetBuffering hWriteTo NoBuffering
  client <- Twitch.connect
  forkIO $ processActions script hReadFrom hWriteTo client
  Twitch.authenticate client nick pass
  Twitch.joinChannel client channel hWriteTo
  Twitch.processMessages client hWriteTo

processActions :: String -> Handle -> Handle -> Twitch.Client -> IO ()
processActions name handleIn handleOut client = forever $ do
  Twitch.Output action <- read <$> hGetLine handleIn
  case action of
    Twitch.SendMessage channel message -> Twitch.sendMessage client channel message
    Twitch.Join channel -> Twitch.joinChannel client channel handleOut
    Twitch.Log message -> hPutStrLn stdout $ (T.pack ("<" ++ name ++ "> ")) `mappend` message
