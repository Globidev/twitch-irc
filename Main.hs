module Main where

import System.Environment (getArgs)

import Text.Printf (printf)

import Control.Monad (when)

import Irc.Client as Irc
import Irc.Parser as Irc

-- Next step is to use Twitch API to fetch this
nick = "globinette"
pass = "oauth:yw9vubelhjv98rlufly6vsbrt7e647"

main :: IO ()
main = do
  args <- getArgs
  when (length args >= 1) $ do
    let channel = (args !! 0)
    client <- Irc.connect
    Irc.authenticate client nick pass
    Irc.joinChannel client channel onMessage

onMessage :: String -> Irc.Message -> Irc.Client -> IO ()
onMessage channel msg client = case msg of
  Irc.PrivateMessage _ sender content -> printf "[MSG] %s: %s\n" sender content
  Irc.JoinMessage _ user              -> printf "[JOIN] %s\n" user
  Irc.PartMessage _ user              -> printf "[PART] %s\n" user
  Irc.ServerMessage _ code content    -> printf "[SERVER] %d: %s\n" code content
  Irc.JtvCommand cmd content          -> printf "[JTV] %s: %s\n" cmd content
  Irc.JtvMode _ mode user             -> printf "[MODE] %s: %s\n" user mode
  Irc.Ping pong                       -> sendPong client pong
