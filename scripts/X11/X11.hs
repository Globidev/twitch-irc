{-# LANGUAGE ForeignFunctionInterface #-}

module X11 (
  getDisplay
, closeDisplay
, getRootWindow
, getFocusedWindow
, sendInputs
, KeyCode
) where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

type XDisplay = Ptr ()
type XStatus = CInt
type XWindow = CULong

foreign import ccall "XOpenDisplay"   openDisplay   :: Ptr Char -> IO XDisplay
foreign import ccall "XCloseDisplay"  closeDisplay  :: XDisplay -> IO CInt
foreign import ccall "XFlush"         flush         :: XDisplay -> IO CInt
foreign import ccall "XRootWindow"    rootWindow    :: XDisplay -> CInt        -> IO XWindow
foreign import ccall "XGetInputFocus" getInputFocus :: XDisplay -> Ptr XWindow -> Ptr (CInt) -> IO CInt
foreign import ccall "XSetInputFocus" setInputFocus :: XDisplay -> XWindow     -> CInt       -> CULong -> IO CInt
foreign import ccall "sendInput"      sendInput     :: XDisplay -> XWindow     -> XWindow    -> CInt   -> CInt    -> IO XStatus

getDisplay :: IO XDisplay
getDisplay = openDisplay nullPtr

getRootWindow :: XDisplay -> IO XWindow
getRootWindow display = rootWindow display 0

getFocusedWindow :: XDisplay -> IO XWindow
getFocusedWindow display = do
  alloca $ \winPtr -> do
    alloca $ \revertPtr -> do
      getInputFocus display winPtr revertPtr
      peek winPtr

type KeyCode = Int
sendInputs :: XDisplay -> XWindow -> XWindow -> [KeyCode] -> IO ()
sendInputs display window root keyCodes = do
  setInputFocus display window 0 0
  mapM_ (press 1) keyCodes
  mapM_ (press 0) keyCodes
  flush display
  return ()
  where
    press state key = sendInput display window root state (fromIntegral key)
