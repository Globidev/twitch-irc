module Script (onMessage) where

import Irc.Client as Irc
import Irc.Parser as Irc

import Text.Printf (printf)

onMessage :: Irc.MessageCallback
onMessage channel msg client = case msg of
  Irc.PrivateMessage _ sender content -> printf "[MSG] %s: %s\n" sender content
  Irc.JoinMessage _ user              -> printf "[JOIN] %s\n" user
  Irc.PartMessage _ user              -> printf "[PART] %s\n" user
  Irc.ServerMessage _ code content    -> printf "[SERVER] %d: %s\n" code content
  Irc.JtvCommand cmd content          -> printf "[JTV] %s: %s\n" cmd content
  Irc.JtvMode _ mode user             -> printf "[MODE] %s: %s\n" user mode
  Irc.Ping pong                       -> Irc.sendPong client pong
