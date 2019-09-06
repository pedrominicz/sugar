module Eval (
  eval
  ) where

import Sugar

evalList :: Sugar -> [Sugar] -> Integer
evalList (Identifier "*") xs = product $ map eval xs
evalList (Identifier "+") xs = sum $ map eval xs
evalList _ _                 = error "Eval.evalList: undefined"

eval :: Sugar -> Integer
eval (List [])     = error "Eval.eval: empty list"
eval (List (x:xs)) = evalList x xs
eval (Number x)    = x
eval _             = error "Eval.eval: undefined"
