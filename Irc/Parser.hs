module Irc.Parser(
  parseMessage
, Message (
            PrivateMessage
          , JoinMessage
          , PartMessage
          , ServerMessage
          , JtvCommand
          , JtvMode
          , Ping
          )
) where

import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)

import Irc.Twitch as Twitch


type Channel = String
type User = String
type Command = String
type Mode = String

data Message = PrivateMessage Channel User String
             | JoinMessage Channel User
             | PartMessage Channel User
             | ServerMessage (Maybe Channel) Int String
             | JtvCommand Command String
             | JtvMode Channel Mode User
             | Ping String deriving (Show)

rest :: Parser String
rest = manyTill anyChar eof

nickname :: Parser String
nickname = many (alphaNum <|> char '_')

username :: Parser User
username = nickname

channel :: Parser Channel
channel = do
  char '#' >> many (alphaNum <|> char '_')

commandPrefix :: Parser User
commandPrefix = do
  char ':' >> nickname >> char '!'
  user <- username
  char '@' >> nickname >> char '.' >> string Twitch.tmi >> space
  return user

privateMessage :: Parser Message
privateMessage = do
  user <- commandPrefix
  string "PRIVMSG" >> space
  chan <- channel
  space >> char ':'
  content <- rest
  return $ PrivateMessage chan user content

joinMessage :: Parser Message
joinMessage = do
  user <- commandPrefix
  string "JOIN" >> space
  chan <- channel
  return $ JoinMessage chan user

partMessage :: Parser Message
partMessage = do
  user <- commandPrefix
  string "PART" >> space
  chan <- channel
  return $ JoinMessage chan user

serverMessage :: Parser Message
serverMessage = do
  char ':' >> (string Twitch.tmi <|> (username >> char '.' >> string Twitch.tmi)) >> space -- didn't find a better way...
  code <- many1 digit
  space >> username >> space
  mbChan <- optionMaybe (optional (char '=' >> space) >> channel)
  case mbChan of -- kind of ugly here too
    Just _ -> space >> char ':'
    Nothing -> char ':'
  content <- rest
  return $ ServerMessage mbChan (read code) content

jtvCommand :: Parser Message
jtvCommand = do
  ((try $ string (":jtv!jtv@jtv." ++ Twitch.tmi)) <|> string ":jtv") >> space -- still not great
  string "PRIVMSG" >> space
  username >> space >> char ':'
  cmd <- many1 upper
  space
  content <- rest
  return $ JtvCommand cmd content

jtvMode :: Parser Message
jtvMode = do
  string ":jtv" >> space
  string "MODE" >> space
  chan <- channel
  space
  mode <- string "+o" -- only this mode for now
  space
  user <- username
  return $ JtvMode chan mode user

pingMessage :: Parser Message
pingMessage = do
  string "PING" >> space
  message <- rest
  return $ Ping message

ircMessage :: Parser Message
ircMessage =  try privateMessage
          <|> try joinMessage
          <|> try partMessage
          <|> try serverMessage
          <|> try jtvCommand
          <|> try jtvMode
          <|> pingMessage

parseMessage :: String -> Either ParseError Message
parseMessage = parse ircMessage ""
