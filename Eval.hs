module Eval where

import Sugar

eval :: Sugar -> Sugar
eval (App x y) =
  case eval x of
    Lam x' expr -> eval $ substitute x' y expr
    x'          -> App x' y
eval x = x

substitute :: Name -> Sugar -> Sugar -> Sugar
substitute x y (Var x')
  | x == x'   = y
  | otherwise = (Var x')
substitute x y (App x' y') = App (substitute x y x') (substitute x y y')
substitute x y (Lam x' y') = Lam x' (substitute x y y')
substitute _ _ x = x
