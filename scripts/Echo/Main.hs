module Main (main) where

import Twitch

import Control.Monad (forever)
import Prelude hiding (log)

import System.IO (hSetBuffering, BufferMode(..), stdout)
import Data.Monoid (mappend)
import qualified Data.Text as T

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  forever $ do
    Input msg <- read <$> getLine
    case msg of
      PrivateMessage _ sender content _ -> log $ T.pack ("[MSG] " ++ sender ++ ": ") `mappend` content
      JoinMessage _ user                -> log $ T.pack ("[JOIN] " ++ user)
      PartMessage _ user                -> log $ T.pack ("[PART] " ++ user)
      ServerMessage _ code content      -> log $ T.pack ("[SERVER] " ++ (show code) ++ ": ") `mappend` content
      JtvCommand cmd content            -> log $ T.pack ("[JTV] " ++ cmd ++ ": ") `mappend` content
      JtvMode _ mode user               -> log $ T.pack ("[MODE] " ++ user ++ ": " ++ mode)

log :: T.Text -> IO ()
log message = putStrLn $ show $ Output $ Log message
