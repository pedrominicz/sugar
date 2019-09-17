module Eval
  ( eval
  ) where

import Expr

eval :: [(String, Expr)] -> Expr -> Maybe Expr
eval _ (Ref _)      = Nothing
eval env (Global x) = lookup x env
eval env (App x x') =
  case eval env x of
    Just (Lam _ body) -> eval env $ substitute 0 x' body
    _                 -> Nothing
eval _ x            = Just x

substitute :: Int -> Expr -> Expr -> Expr
substitute i x (Ref i')
  | i == i' = x
  | i < i'  = Ref (i' - 1)
substitute i x (Lam t body) = Lam t (substitute (i + 1) x body)
substitute i x (App x' y')  = App (substitute i x x') (substitute i x y')
substitute _ _ x            = x
