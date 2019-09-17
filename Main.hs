module Main where

import Check
import Eval
import Expr
import Parse

import Data.Char (isSpace)
import System.Console.Haskeline

runExpr :: Environment -> Expr -> Maybe (Expr, Type)
runExpr env expr = do
  expr'  <- substituteEnv env expr
  ty     <- check expr'
  result <- eval expr'
  pure (result, ty)

repl :: Environment -> InputT IO ()
repl env = do
  maybeInput <- getInputLine "> "
  case maybeInput of
    Nothing -> pure ()
    Just input
      | dropWhile isSpace input == "" -> repl env
      | otherwise -> do
        case parse input of
          Just (Expr expr) -> do
            case runExpr env expr of
              Just (result, ty) -> outputStrLn $ show result ++ " # " ++ show ty
              Nothing           -> outputStrLn "# invalid expression"
            repl env
          Just (Let x expr) -> do
            case runExpr env expr of
              Just (result, _) -> repl ((x, result):env)
              Nothing          -> do
                outputStrLn "# invalid expression"
                repl env
          Nothing -> do
            outputStrLn "# parse error"
            repl env

main :: IO ()
main = runInputT defaultSettings (repl [])
