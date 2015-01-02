module Script (onMessage) where

import Irc.Client (MessageCallback, sendPong)
import Irc.Parser (Message(..))

import Text.Printf (printf)

onMessage :: MessageCallback
onMessage channel msg client = case msg of
  PrivateMessage _ sender content -> printf "[MSG] %s: %s\n" sender content
  JoinMessage _ user              -> printf "[JOIN] %s\n" user
  PartMessage _ user              -> printf "[PART] %s\n" user
  ServerMessage _ code content    -> printf "[SERVER] %d: %s\n" code content
  JtvCommand cmd content          -> printf "[JTV] %s: %s\n" cmd content
  JtvMode _ mode user             -> printf "[MODE] %s: %s\n" user mode
  Ping pong                       -> sendPong client pong
