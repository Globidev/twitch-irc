module Irc.Parser(
  parseMessage
, Message (PrivateMessage, Ping)
) where

import Text.Parsec
import Text.ParserCombinators.Parsec

data Message = PrivateMessage String String
             | Ping String

nickname :: Parser String
nickname = many (alphaNum <|> char '_')

username :: Parser String
username = nickname

privateMessage :: String -> Parser Message
privateMessage channelName = do
  char ':' >> nickname >> char '!'
  u <- username
  char '@' >> nickname >> manyTill anyChar (string ("PRIVMSG #" ++ channelName ++ " :"))
  c <- manyTill anyChar eof
  return $ PrivateMessage u c

pingMessage :: Parser Message
pingMessage = do
  string "PING "
  m <- manyTill anyChar eof
  return $ Ping m

ircMessage :: String -> Parser Message
ircMessage channel =  privateMessage channel
                  <|> pingMessage

parseMessage :: String -> String -> Either ParseError Message
parseMessage channel = parse (ircMessage channel) ""
