module Eval
  ( Environment
  , eval
  , substituteEnv
  ) where

import Expr

type Environment = [(String, Expr)]

eval :: Expr -> Maybe Expr
eval (Ref _)    = Nothing
eval (Global _) = Nothing
eval (App x x') =
  case eval x of
    Just (Lam _ body) -> eval $ substitute 0 x' body
    _                 -> Nothing
eval x          = Just x

substitute :: Int -> Expr -> Expr -> Expr
substitute i x (Ref i')
  | i == i' = x
  | i < i'  = Ref (i' - 1)
substitute i x (Lam t body) = Lam t (substitute (i + 1) x body)
substitute i x (App x' y')  = App (substitute i x x') (substitute i x y')
substitute _ _ x            = x

substituteEnv :: Environment -> Expr -> Maybe Expr
substituteEnv env (Global x) = lookup x env
substituteEnv env (Lam t x) = do
  x' <- substituteEnv env x
  pure $ Lam t x'
substituteEnv env (App x y) = do
  x' <- substituteEnv env x
  y' <- substituteEnv env y
  pure $ App x' y'
substituteEnv _ x = Just x
