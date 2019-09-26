module Main where

import Eval
import Expr
import Infer
import Parse
import Repl
import Type
import Value

import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map as M

import System.Console.Haskeline

runStatement :: Statement -> Repl ()
runStatement (Let x expr) = do
  scheme <- runExceptT (infer expr)
  case scheme of
    Left e -> showStrLn e
    Right scheme'@(Forall _ t) -> do
      result <- runExceptT (eval expr)
      case result of
        Left e -> showStrLn e
        Right result' -> do
          modify $ M.insert x (result', scheme')
          showStrLn $ x ++ " : " ++ show t
runStatement (Expr expr) = do
  scheme <- runExceptT (infer expr)
  case scheme of
    Left e -> showStrLn e
    Right (Forall _ t) -> do
      result <- runExceptT (eval expr)
      case result of
        Left e              -> showStrLn e
        Right (Closure _ _) -> showStrLn $ show t
        Right result'       -> showStrLn $ show result'

repl :: Repl ()
repl = do
  input <- readInput "> "
  case runExcept (parse input) of
    Left e          -> showStrLn e
    Right statement -> runStatement statement
  repl

main :: IO ()
main = runInputT defaultSettings (runRepl repl)
