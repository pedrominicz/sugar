module Main where

import Check
import Eval
import Expr
import Parse

import Control.Monad.State
import Data.Char (isSpace)
import System.Console.Haskeline

runExpr :: Expr -> StateT Environment Maybe (Expr, Type)
runExpr expr = do
  ty     <- check expr
  result <- eval expr
  return (result, ty)

repl :: Environment -> InputT IO ()
repl env = do
  maybeInput <- getInputLine "> "
  case maybeInput of
    Nothing                  -> return ()
    Just input | blank input -> repl env
    Just input -> do
      case parse input of
        Just (Expr expr) -> do
          case evalStateT (runExpr expr) env of
            Just (result, ty) -> outputStrLn $ show result ++ " # " ++ show ty
            Nothing           -> outputStrLn "# invalid expression"
          repl env
        Just (Let x expr) -> do
          case evalStateT (runExpr expr) env of
            Just (expr', _) -> repl $ (x, expr'):env
            Nothing         -> do
              outputStrLn "# invalid expression"
              repl env
        Nothing -> do
          outputStrLn "# parse error"
          repl env

blank :: String -> Bool
blank = (== "") . dropWhile isSpace

main :: IO ()
main = runInputT defaultSettings (repl [])
