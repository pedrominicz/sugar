module Eval (Value, eval) where

import Expr

import Safe (atMay)

data Value
  = Closure [Value] Expr
  | Number Integer
  | Boolean Bool

instance Show Value where
  show (Closure _ _) = "<closure>"
  show (Number x)    = show x
  show (Boolean x)   = if x then "true" else "false"

eval :: [Value] -> Expr -> Maybe Value
eval env (Ref x)   = atMay env x
eval env (Lam t x) = Just $ Closure env (Lam t x)
eval env (App x y) = do
  x' <- eval env x
  y' <- eval env y
  apply x' y'
eval _ (Num x)     = Just $ Number x
eval _ (Bool x)    = Just $ Boolean x
eval _ _           = Nothing

apply :: Value -> Value -> Maybe Value
apply (Closure env (Lam _ body)) x = eval (x:env) body
apply _ _                          = Nothing
