module Main (main) where

import Prelude hiding (log)
import System.IO (hSetBuffering, BufferMode(..), stdout)
import Control.Monad (forever, when)
import Control.Concurrent (threadDelay)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

import Twitch
import qualified X11 as X

inputs :: [(String, [X.KeyCode])]
inputs =
  [ ("up",         [0xff52])
  , ("down",       [0xff54])
  , ("left",       [0xff51])
  , ("right",      [0xff53])
  , ("up+down",    [0xff52, 0xff54])
  , ("up+left",    [0xff52, 0xff51])
  , ("up+right",   [0xff52, 0xff53])
  , ("down+left",  [0xff54, 0xff51])
  , ("down+right", [0xff54, 0xff53])
  , ("left+right", [0xff51, 0xff53])
  , ("enter",      [0xff0d])
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
      PrivateMessage _ sender content _ -> do
        let match = Map.lookup content (Map.fromList inputs)
        case match of
          Just keyCodes -> X.sendInputs display window root keyCodes
          Nothing       -> checkCommands channel content
      _ -> return ()

  X.closeDisplay display
  return ()

log :: String -> IO ()
log msg = putStrLn $ show $ Output $ Log msg

chat :: String -> String -> IO ()
chat channel msg = putStrLn $ show $ Output $ SendMessage channel msg

checkCommands :: String -> String -> IO ()
checkCommands channel message =
  case message of
    "!commands" -> chat channel $ intercalate "  |  " (map fst inputs)
    _           -> return ()
