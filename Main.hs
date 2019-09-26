module Main where

import Eval
import Expr
import Infer
import Parse
import Type
import Value

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe

import Data.Char (isSpace)
import qualified Data.Map as M

import System.Console.Haskeline

type Repl a = MaybeT (StateT Environment (InputT IO)) a

runRepl :: Repl () -> InputT IO ()
runRepl x = do
  _ <- evalStateT (runMaybeT x) M.empty
  return ()

readInput :: String -> Repl String
readInput prompt = do
  maybeInput <- lift . lift $ getInputLine prompt
  case dropWhile isSpace <$> maybeInput of
    Nothing    -> mzero
    Just ""    -> readInput prompt
    Just input -> return input

runStatement :: Statement -> Repl ()
runStatement (Let' x expr) = do
  env <- get
  case runExcept (runReaderT (infer expr) env) of
    Left e -> lift . lift $ outputStrLn e
    Right scheme@(Forall _ t) ->
      case runExcept (runReaderT (eval expr) env) of
        Left e -> lift . lift $ outputStrLn e
        Right result -> do
          put $ M.insert x (result, scheme) env
          lift . lift $ outputStrLn $ x ++ " : " ++ show t
runStatement (Expr expr) = do
  env <- get
  case runExcept (runReaderT (infer expr) env) of
    Left e -> lift . lift $ outputStrLn e
    Right (Forall _ t) ->
      case runExcept (runReaderT (eval expr) env) of
        Left e              -> lift . lift $ outputStrLn e
        Right (Closure _ _) -> lift . lift $ outputStrLn $ show t
        Right result        -> lift . lift $ outputStrLn $ show result

repl :: Repl ()
repl = do
  input <- readInput "> "
  case runExcept (parse input) of
    Left e          -> lift . lift $ outputStrLn e
    Right statement -> runStatement statement
  repl

main :: IO ()
main = runInputT defaultSettings (runRepl repl)
