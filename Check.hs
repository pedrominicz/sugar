module Check
  ( check
  ) where

import Eval
import Expr

import Control.Monad.State
import Safe (atMay)

check :: Expr -> StateT Environment Maybe Type
check = check' []

check' :: [Type] -> Expr -> StateT Environment Maybe Type
check' env (Ref x)   = lift $ atMay env x
check' _ (Global x)  = do
  env' <- get
  x'   <- lift $ lookup x env'
  check' [] x'
check' env (Lam t x) = LamT t <$> check' (t:env) x
check' env (App x y) = do
  tx <- check' env x
  ty <- check' env y
  case tx of
    (LamT ty' t) | ty == ty' -> return t
    _                        -> lift $ Nothing
check' _ (Num _)     = return NumT
check' _ (Bool _)    = return BoolT
