{-# LANGUAGE ForeignFunctionInterface #-}

module Main (main) where

import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import Control.Monad (forever)
import Control.Concurrent (threadDelay)

import Data.Map (Map)
import Data.List (intercalate)
import qualified Data.Map as Map

import Twitch

import Prelude hiding (log)
import System.IO (hSetBuffering, BufferMode(..), stdout)

type XDisplay = ()
type XStatus = CInt
type XWindow = CULong

foreign import ccall "XOpenDisplay"
  xOpenDisplay :: Ptr Char -> Ptr XDisplay
foreign import ccall "XCloseDisplay"
  xCloseDisplay :: Ptr XDisplay -> CInt
foreign import ccall "XRootWindow"
  xRootWindow  :: Ptr XDisplay -> CInt -> XWindow
foreign import ccall "XGetInputFocus"
  xGetInputFocus :: Ptr XDisplay -> Ptr XWindow -> Ptr (CInt) -> CInt
foreign import ccall "XSetInputFocus"
  xSetInputFocus :: Ptr XDisplay -> XWindow -> CInt -> CULong -> CInt
foreign import ccall "sendInput"
  sendInput :: Ptr XDisplay -> XWindow -> XWindow -> CInt -> CInt -> XStatus


inputMap :: Map String [Int]
inputMap = Map.fromList
  [ ("left", [0xff51])
  , ("up", [0xff52])
  , ("right", [0xff53])
  , ("down", [0xff54])
  , ("enter", [0xff0d])
  , ("down+left", [0xff54, 0xff51])
  , ("up+left", [0xff52, 0xff51])
  , ("left+right", [0xff51, 0xff53])
  , ("up+right", [0xff52, 0xff53])
  , ("down+right", [0xff54, 0xff53])
  , ("up+down", [0xff52, 0xff54])
  ]

log :: String -> IO ()
log message = putStrLn $ show $ Output $ Log message

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  let display = xOpenDisplay nullPtr
  let root = xRootWindow display 0

  log "Select the window"

  threadDelay 3000000
  window <- getFocusWindow display
  log $ show window

  forever $ do
    Input channel msg <- read <$> getLine
    case msg of
      PrivateMessage _ sender content -> do
        let match = Map.lookup content inputMap
        case match of
          Just keyCodes -> do
            log $ show $ xSetInputFocus display window 0 0
            log $ show $ sendInputs display window root keyCodes
          Nothing -> do
            if content == "!commands" then do
              let commands = Map.keys inputMap
              let commandMessage = intercalate "  /  " commands
              putStrLn $ show $ Output $ SendMessage channel commandMessage
            else return ()
      _ -> return ()

  let closeSuccess = xCloseDisplay display
  return ()

getFocusWindow :: Ptr XDisplay -> IO XWindow
getFocusWindow display = do
  alloca $ \winPtr -> do
    alloca $ \revertPtr -> do
      let status = xGetInputFocus display winPtr revertPtr
      log $ show status
      peek winPtr

sendInputs :: Ptr XDisplay -> XWindow -> XWindow -> [Int] -> Int
sendInputs display window root keyCodes = do
  let pressed = map (\k -> sendInput display window root 1 (fromIntegral k)) keyCodes
  let released = map (\k -> sendInput display window root 0 (fromIntegral k)) keyCodes
  (sum $ map fromIntegral pressed) + (sum $ map fromIntegral released)
