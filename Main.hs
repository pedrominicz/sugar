module Main where

import Check
import Eval
import Expr
import Parse

import System.Console.Haskeline

evalInput :: String -> Maybe (Expr, Type)
evalInput input = do
  expr   <- parseExpr input
  ty     <- check [] expr
  result <- eval [] expr
  pure (result, ty)

repl :: InputT IO ()
repl = do
  maybeInput <- getInputLine "> "
  case maybeInput of
    Nothing    -> pure ()
    Just input -> do
      case evalInput input of
        Just (result, ty) -> outputStrLn $ show result ++ " : " ++ show ty
        Nothing           -> outputStrLn "invalid expression"
      repl

main :: IO ()
main = runInputT defaultSettings repl
