module Eval where

import Sugar

eval :: Sugar -> Sugar
eval (App x y) =
  case eval x of
    Lam x' y' -> eval $ substitute x' y y'
    x'        -> App x' (eval y)
eval x = x

substitute :: Name -> Sugar -> Sugar -> Sugar
substitute x y a@(Var x')
  | x == x'   = y
  | otherwise = a
substitute x y (App x' y') = App (substitute x y x') (substitute x y y')
substitute x y a@(Lam x' y')
  | x /= x'   = Lam x' (substitute x y y')
  | otherwise = a
