module Main where

import Eval
import Parser

import Data.Map (empty)

repl :: IO ()
repl = do
  input <- readSugar <$> getLine
  case input of
    Left err    -> putStrLn $ show err
    Right sugar ->
      case eval empty sugar of
        Left err     -> putStrLn err
        Right (_, x) -> putStrLn $ show x
  repl

main :: IO ()
main = repl
