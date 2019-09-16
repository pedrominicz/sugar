module Main where

import Check
import Eval
import Parse

import System.Console.Haskeline

repl :: InputT IO ()
repl = do
  maybeInput <- getInputLine "> "
  case maybeInput of
    Nothing    -> pure ()
    Just input -> do
      case parseExpr input >>= (check [] >> eval []) of
        Just result -> outputStrLn $ show result
        Nothing     -> outputStrLn "invalid expression"
      repl

main :: IO ()
main = runInputT defaultSettings repl
