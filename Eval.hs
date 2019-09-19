module Eval
  ( Environment
  , eval
  ) where

import Expr

import Control.Monad.State

type Environment = [(String, Expr)]

eval :: Expr -> StateT Environment Maybe Expr
eval (Ref _)    = lift $ Nothing
eval (Global x) = do
  env <- get
  lift $ lookup x env
eval (App x y) = do
  x' <- eval x
  case x' of
    Lam _ body -> eval $ substitute 0 y body
    _          -> lift $ Nothing
eval x          = return x

substitute :: Int -> Expr -> Expr -> Expr
substitute i x (Ref i')
  | i == i' = x
  | i < i'  = Ref (i' - 1)
substitute i x (Lam t body) = Lam t (substitute (i + 1) x body)
substitute i x (App x' y')  = App (substitute i x x') (substitute i x y')
substitute _ _ x            = x
