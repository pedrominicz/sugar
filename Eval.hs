module Eval (
  eval
  ) where

import Sugar

import Data.Map (empty)

eval :: Context -> Sugar -> Either String (Context, Sugar)
eval _ (List [])  = Left "Eval.eval: empty list"
eval _ (Number x) = Right $ (empty, Number x)
eval _ _          = Left "Eval.eval: undefined"
