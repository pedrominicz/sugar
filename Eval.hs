module Eval (
  eval
  ) where

import Sugar

eval :: Context -> Sugar -> Either String (Context, Sugar)
eval ctx (Identifier x) =
  case lookup x ctx of
    Just f  -> Right (ctx, f [])
    Nothing -> Left $ "Eval.eval: unbound identifier: `" ++ x ++ "`"
eval ctx (List (Identifier x:args)) =
  case lookup x ctx of
    Just f  -> Right (ctx, f args)
    Nothing -> Left $ "Eval.eval: unbound identifier: `" ++ x ++ "`"
eval ctx (Number x) = Right (ctx, Number x)
eval _ _ = Left "Eval.eval: undefined"
