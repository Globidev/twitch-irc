module Twitch.Irc.Types where

import Data.Map (Map)

import System.IO (Handle)

data Input = Input Message deriving (Show, Read)
data Output = Output Action deriving (Show, Read)

data Message
  = PrivateMessage Channel User String (Maybe PrivateMessageTags)
  | JoinMessage Channel User
  | PartMessage Channel User
  | ServerMessage (Maybe Channel) Int String
  | JtvCommand Command String
  | JtvMode Channel Mode User
  | Ping String deriving (Show, Read)

data Action
  = SendMessage Channel String
  | Log String
  | Join Channel deriving (Show, Read)

type Channel = String
type User = String
type Command = String
type Mode = String
type Client = Handle

type PrivateMessageTags = Map String String
