{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Twitch

import System.Environment (getArgs)

import Control.Monad (forever, forM_)
import Control.Concurrent (forkIO, MVar, newMVar, readMVar, modifyMVar_)
import Control.Exception (finally)

import Data.Aeson (ToJSON, toJSON, (.=), object, encode)

import qualified Network.WebSockets as WS

data WSClient = WSClient
  { connection :: WS.Connection
  , uid        :: Int
  }

data ServerState = ServerState
  { clients :: [WSClient]
  , nextUid :: Int
  }

data IRCMessage = IRCMessage
  { sender  :: String
  , content :: String
  , tags    :: Maybe PrivateMessageTags
  } deriving Show

instance ToJSON IRCMessage where
  toJSON IRCMessage{..} = object [ "sender" .= sender
                                 , "content" .= content
                                 , "tags" .= tags
                                 ]

newServerState :: ServerState
newServerState = ServerState
  { clients = []
  , nextUid = 0
  }

newClient :: WS.Connection -> ServerState -> WSClient
newClient conn ServerState{..} = WSClient
  { connection = conn
  , uid        = nextUid
  }

addClient :: WSClient -> ServerState -> ServerState
addClient client ServerState{..} = ServerState
  { clients = (client : clients)
  , nextUid = nextUid + 1
  }

removeClient :: WSClient -> ServerState -> ServerState
removeClient client ServerState{..} = ServerState
  { clients = filter ((/= uid client) . uid) clients
  , nextUid = nextUid
  }

--

broadcastMessages :: MVar ServerState -> IO ()
broadcastMessages state = forever $ do
  Input _ message <- read <$> getLine
  case message of
    PrivateMessage _ sender content tags ->
      broadcast (IRCMessage sender content tags) state
    _ -> return ()

broadcast :: ToJSON a => a -> MVar ServerState -> IO ()
broadcast message state = do
  ServerState clients _ <- readMVar state
  forM_ clients $ \WSClient{..} ->
    WS.sendTextData connection (encode $ toJSON message)

app :: MVar ServerState -> WS.ServerApp
app state pending = do
  conn <- WS.acceptRequest pending
  -- Add the new client
  client <- newClient conn <$> readMVar state
  modifyMVar_ state (return . addClient client)
  -- Talk forever until closed
  (talk conn) `finally` (disconnect client state)
    where
      talk conn = do
        msg <- WS.receiveDataMessage conn
        talk conn
      disconnect client state = modifyMVar_ state (return . removeClient client)

getPort :: IO (Int)
getPort = do
  args <- getArgs
  case args of
    port:_ -> return $ read port
    _      -> return 8080

main :: IO ()
main = do
  port <- getPort
  state <- newMVar newServerState
  forkIO $ broadcastMessages state
  WS.runServer "0.0.0.0" port $ app state
