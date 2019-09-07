module Main where

import Eval
import Parser
import Sugar

import System.Console.Haskeline

repl :: Context -> InputT IO ()
repl ctx = do
  maybeInput <- getInputLine "> "
  case maybeInput of
    Nothing    -> return ()
    Just input -> do
        let (ctx', result) = eval ctx $ readSugar input
        outputStrLn $ show result
        repl ctx'

main :: IO ()
main = runInputT defaultSettings (repl [])
