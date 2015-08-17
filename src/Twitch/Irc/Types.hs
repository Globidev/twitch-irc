{-# LANGUAGE DeriveDataTypeable #-}
module Twitch.Irc.Types where

import Data.Typeable.Internal (Typeable)
import System.IO (Handle)

data Input = Input Channel Message deriving (Show, Read, Eq)
data Output = Output Action deriving (Show, Read, Eq)

data Message
  = PrivateMessage Channel User String
  | JoinMessage Channel User
  | PartMessage Channel User
  | ServerMessage (Maybe Channel) Int String
  | JtvCommand Command String
  | JtvMode Channel Mode User
  | Ping String deriving (Show, Read, Eq, Typeable)

data Action
  = SendMessage Channel String
  | Log String deriving (Show, Read, Eq)

type Channel = String
type User = String
type Command = String
type Mode = String
type Client = Handle
