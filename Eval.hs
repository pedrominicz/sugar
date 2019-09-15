module Eval where

import Sugar

data Closure
  = Closure Environment Sugar
  deriving Show

type Environment = [(Name, Closure)]

eval :: Environment -> Sugar -> Closure
eval env (Var x) =
  case lookup x env of
    Just (Closure _ x') -> Closure env x'
    Nothing             -> Closure env (Var x)
eval env (App x y) = apply (eval env x) (eval env y)
eval env x         = Closure env x

apply :: Closure -> Closure -> Closure
apply (Closure env (Lam x body)) y  = eval ((x,y):env) body
apply (Closure env x) (Closure _ y) = Closure env (App x y)
