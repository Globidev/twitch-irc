module Main where

import System.Environment (getArgs)

import Text.Printf (printf)

import Control.Monad (when)

import Irc.Client as Irc
import Irc.Parser as Irc

server = "irc.twitch.tv"
port = 6667
nick = "globinette"
pass = "oauth:yw9vubelhjv98rlufly6vsbrt7e647"

main :: IO ()
main = do
  args <- getArgs
  when (length args >= 1) $ do
    let channel = (args !! 0)
    client <- Irc.connect server port
    Irc.authenticate client nick pass
    Irc.joinChannel client channel onMessage

onMessage :: String -> Irc.Message -> Irc.Client -> IO ()
onMessage channel msg client = case msg of
  Irc.PrivateMessage sender content -> printf "%s: %s\n" sender content
  Irc.Ping m -> sendPong client m
