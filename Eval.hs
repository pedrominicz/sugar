module Eval (
  eval
  ) where

import Sugar

import qualified Data.Text as T

evalList :: Sugar -> [Sugar] -> Integer
evalList (Identifier x) xs = case T.unpack x of
  "*" -> product $ map eval xs
  "+" -> sum $ map eval xs
  _   -> error "Eval.evalList: undefined identifier"
evalList _ _ = error "Eval.evalList: undefined"

eval :: Sugar -> Integer
eval s = case s of
  List (x:xs) -> evalList x xs
  Number x    -> x
  _           -> error "Eval.eval: undefined"
