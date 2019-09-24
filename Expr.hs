module Expr
  ( Name
  , Expr(..)
  , Statement(..)
  ) where

import Type

type Name = String

data Statement
  = Let' Name Expr
  | Expr Expr
  deriving Show

data Expr
  = Ref Int
  | Global Name
  | Lam (Maybe Type) Expr
  | App Expr Expr
  | Num Integer
  | Bool Bool
  | Let Expr Expr
  deriving Show
