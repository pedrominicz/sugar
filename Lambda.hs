module Lambda where

type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  deriving Show

apply :: Name -> Expr -> Expr -> Expr
apply x (Var x') e
  | x == x'   = e
  | otherwise = Var x'
apply x (App x' e') e = App (apply x x' e') (apply x e' e)
apply x (Lam x' e') e = Lam x' (apply x e' e)
