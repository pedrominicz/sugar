module Eval
  ( eval
  ) where

import Expr
import Value

import Control.Monad.Except
import Control.Monad.State

import Safe (atMay)

eval :: Expr -> StateT Environment (Except String) Value
eval = eval' []

eval' :: [Value] -> Expr -> StateT Environment (Except String) Value
eval' env (Ref x) =
  case atMay env x of
    Just x' -> return x'
    Nothing -> error $ "Eval.eval': unbound reference: " ++ show x
eval' _ (Global x) = do
  env <- get
  case lookup x env of
    Just (_, x') -> return x'
    Nothing      -> error $ "Eval.eval': unbound variable: " ++ x
eval' env (Lam _ x) = return $ Closure env (Lam Nothing x)
eval' env (App x y) = do
  x' <- eval' env x
  y' <- eval' env y
  apply x' y'
eval' _ (Num x)     = return $ Number x
eval' _ (Bool x)    = return $ Boolean x
eval' env (Let e x) = do
  e' <- eval' env e
  apply (Closure env (Lam Nothing x)) e'

apply :: Value -> Value -> StateT Environment (Except String) Value
apply (Closure env (Lam _ body)) x = eval' (x:env) body
apply _ _                          = error $ "Eval.apply: not a closure"
