module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Process

import Control.Monad (when)
import qualified Twitch

-- Next step is to use Twitch API to fetch this
nick = "globinette"
pass = "oauth:c6gnjdi9tmqysf4edozkim16l77605"

main :: IO ()
main = do
  args <- getArgs
  when (length args >= 2) $ do
    let channel = (args !! 0)
    let script = (args !! 1)
    run channel script

run :: String -> String -> IO ()
run channel scriptFile = do
  (Just hin, Just hout, _, _) <- createProcess (proc "ls" []){ std_out = CreatePipe }
  r <- createProcess (proc "ls" [])
  client <- Twitch.connect
  Twitch.authenticate client nick pass
  Twitch.joinChannel client channel hin
  
