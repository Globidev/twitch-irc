module Main (main) where

import Control.Monad (forever, when)
import Control.Concurrent (threadDelay)

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

import Prelude hiding (log)
import System.IO (hSetBuffering, BufferMode(..), stdout)

import Twitch
import qualified X11 as X

inputMap :: Map String [Int]
inputMap = Map.fromList
  [ ("left",       [0xff51])
  , ("up",         [0xff52])
  , ("right",      [0xff53])
  , ("down",       [0xff54])
  , ("enter",      [0xff0d])
  , ("down+left",  [0xff54, 0xff51])
  , ("up+left",    [0xff52, 0xff51])
  , ("left+right", [0xff51, 0xff53])
  , ("up+right",   [0xff52, 0xff53])
  , ("down+right", [0xff54, 0xff53])
  , ("up+down",    [0xff52, 0xff54])
  ]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- This should be piped so: no buffering

  display <- X.getDisplay
  root <- X.getRootWindow display
  log "3 seconds to select a window"
  threadDelay 3000000
  window <- X.getFocusedWindow display
  log $ "Selected window: " ++ show window

  forever $ do
    Input channel msg <- read <$> getLine
    case msg of
      PrivateMessage _ sender content -> do
        case Map.lookup content inputMap of
          Just keyCodes -> do
            X.sendInputs display window root keyCodes
          Nothing -> do
            when (content == "!commands") $ do
              let commandsText = (intercalate "  /  " (Map.keys inputMap))
              chat channel commandsText
      _ -> return ()

  X.closeDisplay display
  return ()

log :: String -> IO ()
log msg = putStrLn $ show $ Output $ Log msg

chat :: String -> String -> IO ()
chat channel msg = putStrLn $ show $ Output $ SendMessage channel msg
