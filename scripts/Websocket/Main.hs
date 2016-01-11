{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Twitch

import GHC.Generics

import Control.Monad (forever, unless, forM_)
import Control.Concurrent (forkIO, threadDelay, MVar, newMVar, modifyMVar_, readMVar)

import System.IO (hSetBuffering, BufferMode(..), stdout)

import Data.Aeson

import qualified Network.WebSockets as WS

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

type WSClient = WS.Connection
type ServerState = [WSClient]

data IRCMessage = IRCMessage
  {
    sender  :: String
  , content :: String
  } deriving Show

instance ToJSON IRCMessage where
  toJSON (IRCMessage sender content) =
    object ["sender"  .= sender, "content" .= content ]

newServerState :: ServerState
newServerState = []

addClient :: WSClient -> ServerState -> ServerState
addClient client clients = client : clients

broadcast :: ToJSON a => a -> ServerState -> IO ()
broadcast message clients = do
  forM_ clients $ \client ->
    WS.sendTextData client (encode $ toJSON message)

app :: MVar ServerState -> WS.ServerApp
app state pending = do
  conn <- WS.acceptRequest pending
  modifyMVar_ state $ \s -> do
    return $ addClient conn s
  forever $ threadDelay 30000000

broadcastMessages :: MVar ServerState -> IO ()
broadcastMessages state = forever $ do
  Input channel msg <- read <$> getLine
  clients <- readMVar state
  case msg of
    PrivateMessage _ sender content -> broadcast (IRCMessage sender content) clients
    _ -> return ()

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  state <- newMVar newServerState
  forkIO $ broadcastMessages state
  WS.runServer "0.0.0.0" 8080 $ app state
