module Eval where

import Sugar

data Clojure
  = Clojure Environment Sugar
  deriving Show

type Environment = [(Name, Clojure)]

eval :: Environment -> Sugar -> Clojure
eval env (Var x) =
  case lookup x env of
    Just (Clojure _ x') -> Clojure env x'
    Nothing             -> Clojure env (Var x)
eval env (App x y) = apply (eval env x) (eval env y)
eval env x         = Clojure env x

apply :: Clojure -> Clojure -> Clojure
apply (Clojure env (Lam x body)) y = eval ((x,y):env) body
apply _ _ = error "Eval.apply: first argument is not a lambda"
