module Main where

import Eval
import Parser

import qualified Data.Text as T

main :: IO ()
main = do
  sugar <- readSugar . T.pack <$> getLine
  putStrLn $ case sugar of
    Left err -> show err
    Right x  -> show $ eval x
