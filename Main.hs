module Main where

import Network

import System.Environment
import System.IO

import Text.Printf
import Text.Parsec
import Text.ParserCombinators.Parsec

import Control.Monad

-- Parsing stuff
data IrcMessage = IrcMessage { user :: String
                             , content :: String
                             }

nickname :: Parser String
nickname = many (alphaNum <|> char '_')

username :: Parser String
username = nickname

ircMessage :: String -> Parser IrcMessage
ircMessage channelName = do
  char ':' >> nickname >> char '!'
  u <- username
  char '@' >> nickname >> string (".tmi.twitch.tv PRIVMSG #"++channelName++" :")
  c <- manyTill anyChar eof
  return $ IrcMessage u c

--
server = "irc.twitch.tv"
port = PortNumber 6667

oauth = "yw9vubelhjv98rlufly6vsbrt7e647"

main :: IO ()
main = do
  args <- getArgs
  if length args >= 1
    then connectToChannel (args !! 0)
    else return ()

connectToChannel :: String -> IO ()
connectToChannel channelName = do
  socket <- connectTo server port
  hSetBuffering socket NoBuffering
  -- Authentication
  write socket "PASS" ("oauth:"++oauth)
  write socket "NICK" "globinette"
  write socket "JOIN" ("#"++channelName)
  -- Listen forever
  forever $ do
    s <- hGetLine socket
    case parse (ircMessage channelName) "" s of
      Right msg -> handleMessage channelName socket msg
      Left _ -> putStrLn s

write :: Handle -> String -> String -> IO ()
write h s t = do
  hPrintf h "%s %s\r\n" s t
  printf    "> %s %s\n" s t

handleMessage :: String -> Handle -> IrcMessage -> IO ()
handleMessage channelName h (IrcMessage u c) = do
  if u == "twitchnotify"
    then
      case parse username "" c of
        Right name -> hPrintf h "PRIVMSG #%s :Welcome %s! Thank you for subscribing\r\n" channelName name
        Left _ -> return ()
    else printf "%s: %s\n" u c
