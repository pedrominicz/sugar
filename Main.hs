module Main where

import Eval
import Parser

import System.Console.Haskeline

repl :: InputT IO ()
repl = do
  maybeInput <- getInputLine "> "
  case maybeInput of
    Nothing    -> pure ()
    Just input -> do
      outputStrLn $ case eval [] $ parseExpr input of
        Just result -> show result
        Nothing     -> "invalid expression"
      repl

main :: IO ()
main = runInputT defaultSettings repl
