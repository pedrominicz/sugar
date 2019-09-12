module Main where

import Eval

import System.Console.Haskeline

repl :: Environment -> InputT IO ()
repl env = do
  maybeInput <- getInputLine "> "
  case maybeInput of
    Nothing    -> return ()
    Just input -> do
        let (Clojure env' result) = eval env $ read input
        outputStrLn $ show result
        repl env'

main :: IO ()
main = runInputT defaultSettings (repl [])
