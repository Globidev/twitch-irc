module Main (main) where

import Twitch

import Control.Monad (forever)
import Data.IORef
import Text.Printf (printf)

main :: IO ()
main = do
  a <- newIORef (0::Int)
  forever $ do
     Input channel msg <- read <$> getLine 
     case msg of
        PrivateMessage _ sender content -> printf "[MSG] %s: %s\n" sender content
        JoinMessage _ user              -> do
          count <- readIORef a
          printf "[JOIN] %i %s\n" count user
          writeIORef a (count+1)
        PartMessage _ user              -> printf "[PART] %s\n" user
        ServerMessage _ code content    -> printf "[SERVER] %d: %s\n" code content
        JtvCommand cmd content          -> printf "[JTV] %s: %s\n" cmd content
        JtvMode _ mode user             -> printf "[MODE] %s: %s\n" user mode
        Ping pong                       -> print "pong"

