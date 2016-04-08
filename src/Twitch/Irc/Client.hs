module Twitch.Irc.Client(
  Client
, connect
, authenticate
, joinChannel
, sendMessage
, sendPong
, processMessages
) where

import Network (connectTo, PortID(PortNumber))
import System.IO (
  Handle, hSetBuffering, BufferMode(NoBuffering), stderr, hPrint)
import Text.Printf (hPrintf)
import Data.Text.IO (hGetLine, hPutStrLn)
import Data.Monoid (mappend)
import Control.Monad (forever)

import Twitch.Irc.Parser
import Twitch.Irc.Constants as Twitch
import Twitch.Irc.Types

import qualified Data.Text as T


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
  sendCommand client "CAP REQ" ":twitch.tv/tags"

joinChannel :: Client -> String -> Handle -> IO ()
joinChannel client channel handle = do
  sendCommand client "JOIN" ("#" ++ channel)

sendMessage :: Client -> String -> T.Text -> IO ()
sendMessage client channel msg = do
  let formatted = "#" ++ channel ++ " :" ++ (T.unpack msg)
  sendCommand client "PRIVMSG" formatted

sendPong :: Client -> T.Text -> IO ()
sendPong client msg = sendMessage client "PONG" msg

processMessages :: Client -> Handle -> IO ()
processMessages client handle = forever $ do
  line <- hGetLine client
  case parseMessage line of
    Right (Ping pong) -> sendPong client pong
    Right msg -> hPrint handle (Input msg)
    Left _ -> hPutStrLn stderr ((T.pack "Error parsing:") `mappend` line)
