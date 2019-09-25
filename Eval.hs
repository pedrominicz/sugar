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
eval' env (Let e x) = do
  e' <- eval' env e
  apply (Closure env (Lam Nothing x)) e'
eval' _ (Num x)  = return $ Number x
eval' _ (Bool x) = return $ Boolean x
eval' env (Op op x y) = do
  x' <- eval' env x
  y' <- eval' env y
  return $ arith op x' y'

apply :: Value -> Value -> StateT Environment (Except String) Value
apply (Closure env (Lam _ body)) x = eval' (x:env) body
apply _ _                          = error "Eval.apply: not a closure"

arith :: Op -> Value -> Value -> Value
arith Add      (Number x) (Number y) = Number (x + y)
arith Sub      (Number x) (Number y) = Number (x - y)
arith Mul      (Number x) (Number y) = Number (x * y)
arith Div      (Number x) (Number y) = Number (x `div` y)
arith Mod      (Number x) (Number y) = Number (x `mod` y)
arith Less     (Number x) (Number y) = Boolean (x < y)
arith LessE    (Number x) (Number y) = Boolean (x <= y)
arith Greater  (Number x) (Number y) = Boolean (x > y)
arith GreaterE (Number x) (Number y) = Boolean (x >= y)
arith Equals   (Number x) (Number y) = Boolean (x == y)
arith _ _ _ = error "Eval.arith: not a number"
