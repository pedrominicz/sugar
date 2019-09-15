module Main where

import Eval

import System.Console.Haskeline

repl :: InputT IO ()
repl = do
  maybeInput <- getInputLine "> "
  case maybeInput of
    Nothing    -> return ()
    Just input -> do
      let (Closure _ result) = eval [] $ read input
      outputStrLn $ show result
      repl

main :: IO ()
main = runInputT defaultSettings repl
