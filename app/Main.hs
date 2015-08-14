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
  case args of
    channel:script:_ -> run channel script
    _ -> error "specify channel and script. example: twitch-app lirik twitch-hello-world"
  
run :: String -> String -> IO ()
run channel scriptFile = do
  proc <- createProcess (proc scriptFile []){std_in = CreatePipe, std_out = Inherit}
  -- todo: add std_out = CreatePipe to catch responses
  -- also tweak below as: (Just hin, Just hout, _, _) -> do
  case proc of 
    (Just hin, _, _, _) -> do
      print "ok"
      client <- Twitch.connect
      Twitch.authenticate client nick pass
      Twitch.joinChannel client channel hin
    _ -> error (scriptFile ++ " doesn't exist")


test = do
  proc <- createProcess (proc "twitch-hello-world" []){ std_in = CreatePipe, std_out = Inherit}
  case proc of 
    (Just hin, Just hout, _, _) -> do
      print "ok"
      hPrint hout "ok2"
    _ -> error ("twitch-hello-world" ++ " doesn't exist")
