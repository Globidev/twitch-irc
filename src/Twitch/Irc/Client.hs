module Twitch.Irc.Client(
  Client
, connect
, authenticate
, joinChannel
, sendMessage
, sendPong
) where

import Network (connectTo, PortID(PortNumber))
import System.IO (Handle, hSetBuffering, BufferMode(NoBuffering), hGetLine)
import Text.Printf (hPrintf)
import Control.Monad (forever)
import Twitch.Irc.Parser
import Twitch.Irc.Constants as Twitch
import Twitch.Irc.Types
import System.IO


connect :: IO Client
connect = do
  socket <- connectTo Twitch.server (PortNumber (fromIntegral Twitch.port))
  hSetBuffering socket NoBuffering
  return socket

sendCommand :: Client -> String -> String -> IO ()
sendCommand client = hPrintf client "%s %s\r\n"

authenticate :: Client -> String -> String -> IO ()
authenticate client nick pass = do
  sendCommand client "PASS" pass
  sendCommand client "NICK" nick

joinChannel :: Client -> String -> Handle -> IO ()
joinChannel client channel handle = do
  sendCommand client "JOIN" ("#" ++ channel)
  forever $ do
    line <- hGetLine client
    case parseMessage line of
      Right (Ping pong) -> sendPong client pong
      Right msg -> hPrint handle (Input channel msg)
      Left _ -> putStrLn $ "Error parsing:" ++ line

sendMessage :: Client -> String -> String -> IO ()
sendMessage client channel msg = do
  let formatted = "#" ++ channel ++ " :" ++ msg
  sendCommand client "PRIVMSG" formatted

sendPong :: Client -> String -> IO ()
sendPong client msg = sendMessage client "PONG" msg
