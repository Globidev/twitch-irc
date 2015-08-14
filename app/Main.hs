module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import Control.Monad (when)

import Language.Haskell.Interpreter

import Irc.Client as Irc

-- Next step is to use Twitch API to fetch this
nick = "globinette"
pass = "oauth:c6gnjdi9tmqysf4edozkim16l77605"

main :: IO ()
main = do
  args <- getArgs
  when (length args >= 2) $ do
    let channel = (args !! 0)
    let scriptFile = (args !! 1)
    run channel scriptFile

run :: String -> String -> IO ()
run channel scriptFile = do
  onMessage <- evalCallback scriptFile
  client <- Irc.connect
  Irc.authenticate client nick pass
  Irc.joinChannel client channel onMessage

evalCallback :: String -> IO (Irc.MessageCallback)
evalCallback scriptFile = do
  result <- runInterpreter $ do
    loadModules [scriptFile]
    setImports ["Prelude", "System.IO"]
    setTopLevelModules["Script"]
    interpret "onMessage" (as :: Irc.MessageCallback)
  case result of
    Right callback -> return callback
    Left error -> do
      putStrLn $ "error interpreting: " ++ scriptFile
      print error
      exitFailure
