module Main (main) where

import Twitch

import Control.Monad (forever)
import Prelude hiding (log)

import System.IO (hSetBuffering, BufferMode(..), stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  forever $ do
    Input channel msg <- read <$> getLine
    case msg of
      PrivateMessage _ sender content -> log $ "[MSG] " ++ sender ++ ": " ++ content
      JoinMessage _ user              -> log $ "[JOIN] " ++ user
      PartMessage _ user              -> log $ "[PART] " ++ user
      ServerMessage _ code content    -> log $ "[SERVER] " ++ (show code) ++ ": " ++ content
      JtvCommand cmd content          -> log $ "[JTV] " ++ cmd ++ ": " ++ content
      JtvMode _ mode user             -> log $ "[MODE] " ++ user ++ ": " ++ mode

log :: String -> IO ()
log message = putStrLn $ show $ Output $ Log message
