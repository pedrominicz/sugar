module Eval
  ( eval
  ) where

import Expr
import Value

import Control.Monad.Except
import Control.Monad.Reader

import qualified Data.Map as M

import Safe (atMay)

eval :: Expr -> ReaderT Environment (Except String) Value
eval = eval' []

eval' :: [Value] -> Expr -> ReaderT Environment (Except String) Value
eval' env (Ref x) =
  case atMay env x of
    Just x' -> return x'
    Nothing -> error $ "Eval.eval': unbound reference"
eval' _ (Global x) = do
  env <- ask
  case M.lookup x env of
    Just (x', _) -> return x'
    Nothing      -> error $ "Eval.eval': unbound variable: " ++ x
eval' env (Lam _ x) = return $ Closure env (Lam Nothing x)
eval' env (App x y) = do
  x' <- eval' env x
  y' <- eval' env y
  apply x' y'
eval' env (Let x y) = eval' env (App (Lam Nothing y) x)
eval' _ (Num x)     = return $ Number x
eval' _ (Bool x)    = return $ Boolean x
eval' env (Op op x y) = do
  x' <- eval' env x
  y' <- eval' env y
  return $ arith op x' y'
eval' env (If cond x y) = do
  cond' <- eval' env cond
  case cond' of
    Boolean True  -> eval' env x
    Boolean False -> eval' env y
    _ -> error "Eval.eval: conditional not a boolean"
eval' env (Fix x) = return $ Closure env (Fix x)

apply :: Value -> Value -> ReaderT Environment (Except String) Value
apply (Closure env (Lam _ body)) x = eval' (x:env) body
apply (Closure env (Fix body)) x = do
  body' <- eval' env (App body (Fix body))
  apply body' x
apply _ _ = error "Eval.apply: not a closure"

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
arith _ x y = error $ "Eval.arith: not a number" ++ show x ++ "\n" ++ show y
