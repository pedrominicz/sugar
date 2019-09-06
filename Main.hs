module Main where

import Eval
import Parser

main :: IO ()
main = do
  sugar <- readSugar <$> getLine
  case sugar of
    Left err -> putStrLn $ show err
    Right x  -> putStrLn $ show $ eval x
