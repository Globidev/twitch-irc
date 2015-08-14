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
  proc <- createProcess (proc scriptFile []){std_in = CreatePipe } -- todo: add std_out = CreatePipe to catch responses
  case proc of 
    (Just hin, Just hout, _, _) -> do
      print "ok"
      client <- Twitch.connect
      Twitch.authenticate client nick pass
      Twitch.joinChannel client channel hin
    _ -> error (scriptFile ++ " doesn't exist")


test = do
  proc <- createProcess (proc "twitch-hello-world" []){ std_in = CreatePipe }
  case proc of 
    (Just hin, Just hout, _, _) -> do
      print "ok"
    _ -> error ("twitch-hello-world" ++ " doesn't exist")
