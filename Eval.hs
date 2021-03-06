module Eval
  ( eval
  ) where

import Expr
import Value
import Repl

import Control.Monad.Except
import Control.Monad.Reader

import qualified Data.Map as M

type Eval = ReaderT (M.Map Name Value) (Except String)

eval :: Expr -> Repl (Either String Value)
eval expr = do
  env <- M.map fst <$> ask
  return $ runExcept $ runReaderT (eval' expr) env

eval' :: Expr -> Eval Value
eval' (Var x) = do
  env <- ask
  case M.lookup x env of
    Just x' -> return x'
    Nothing -> error $ "Eval.eval': unbound variable: " ++ x
eval' (Lam x y) = do
  env <- ask
  return $ Closure env (Lam x y)
eval' (App x y) = do
  x' <- eval' x
  y' <- eval' y
  apply x' y'
eval' (Num x)  = return $ Number x
eval' (Bool x) = return $ Boolean x
eval' (Op op x y) = do
  x' <- eval' x
  y' <- eval' y
  arith op x' y'
eval' (If cond x y) = do
  cond <- eval' cond
  case cond of
    Boolean True  -> eval' x
    Boolean False -> eval' y
    _ -> throwError $ "Not a boolean\n  " ++ show cond
eval' (Fix x) = do
  env <- ask
  return $ Closure env (Fix x)

apply :: Value -> Value -> Eval Value
apply (Closure _ (Lam Nothing body)) _ = eval' body
apply (Closure env (Lam (Just x) body)) y =
  local (const $ M.insert x y env) $ eval' body
apply (Closure env (Fix body)) x = do
  body' <- local (const env) $ eval' (App body (Fix body))
  apply body' x
apply _ _ = error "Eval.apply: not a closure"

arith :: Op -> Value -> Value -> Eval Value
arith Add      (Number x) (Number y)   = return $ Number (x + y)
arith Sub      (Number x) (Number y)   = return $ Number (x - y)
arith Mul      (Number x) (Number y)   = return $ Number (x * y)
arith Div      (Number x) (Number y)   = return $ Number (x `div` y)
arith Mod      (Number x) (Number y)   = return $ Number (x `mod` y)
arith Less     (Number x) (Number y)   = return $ Boolean (x < y)
arith LessE    (Number x) (Number y)   = return $ Boolean (x <= y)
arith Greater  (Number x) (Number y)   = return $ Boolean (x > y)
arith GreaterE (Number x) (Number y)   = return $ Boolean (x >= y)
arith Equals   (Number x) (Number y)   = return $ Boolean (x == y)
arith Equals   (Boolean x) (Boolean y) = return $ Boolean (x == y)
arith op x y = throwError $
  "Cannot " ++ verb op ++ "\n  " ++ show x ++ "\nwith\n  " ++ show y

verb :: Op -> String
verb Add = "add"
verb Sub = "subtract"
verb Mul = "multiply"
verb Div = "divide (/)"
verb Mod = "divide (%)"
verb x   = "compare (" ++ show x ++ ")"
