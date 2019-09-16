module Check where

import Expr

import Safe (atMay)

check :: [Type] -> Expr -> Maybe Type
check env (Ref x)   = atMay env x
check _ (Global _)  = Nothing
check env (Lam t x) = LamT t <$> check (t:env) x
check env (App x y) = do
  tx <- check env x
  ty <- check env y
  case tx of
    (LamT ty' t) | ty == ty' -> pure t
    _                        -> Nothing
check _ (Num _)     = pure NumT
check _ (Bool _)    = pure BoolT
