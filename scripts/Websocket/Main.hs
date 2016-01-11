{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Twitch

import Control.Monad (forever, unless, forM_)
import Control.Concurrent (forkIO, MVar, newMVar, modifyMVar_, readMVar)
import Control.Exception (finally)

import System.IO (hSetBuffering, BufferMode(..), stdout)

import Data.Aeson

import qualified Network.WebSockets as WS

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

data WSClient = WSClient
  {
    connection :: WS.Connection
  , uid        :: Int
  }

data ServerState = ServerState
  {
    clients :: [WSClient]
  , nextId  :: Int
  }

data IRCMessage = IRCMessage
  {
    sender  :: String
  , content :: String
  } deriving Show

instance ToJSON IRCMessage where
  toJSON (IRCMessage sender content) =
    object ["sender"  .= sender, "content" .= content ]

newServerState :: ServerState
newServerState = ServerState [] 0

newClient :: WS.Connection -> ServerState -> WSClient
newClient conn state = WSClient conn (nextId state)

addClient :: WSClient -> ServerState -> ServerState
addClient client state = do
  ServerState (client : clients state) (nextId state + 1)

removeClient :: WSClient -> ServerState -> ServerState
removeClient client state = do
  let filtered = filter ((/= uid client) . uid) (clients state)
  ServerState filtered (nextId state)

broadcast :: ToJSON a => a -> ServerState -> IO ()
broadcast message state = do
  forM_ (clients state) $ \client ->
    WS.sendTextData (connection client) (encode $ toJSON message)

app :: MVar ServerState -> WS.ServerApp
app state pending = do
  conn <- WS.acceptRequest pending
  -- Add the new client
  s <-readMVar state
  let client = newClient conn s
  modifyMVar_ state $ \s -> do
    return $ addClient client s
  -- Talk forever until closed
  (talk conn) `finally` (disconnect client state)

  where
  talk conn = do
    msg <- WS.receiveDataMessage conn
    talk conn
  disconnect client state = do
    modifyMVar_ state $ \s -> do
      return $ removeClient client s

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
