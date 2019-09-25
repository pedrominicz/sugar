module Expr
  ( Name
  , Statement(..)
  , Expr(..)
  , Op(..)
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
  | Let Expr Expr
  | Num Integer
  | Bool Bool
  | Op Op Expr Expr
  | If Expr Expr Expr
  deriving Show

data Op
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Less
  | LessE
  | Greater
  | GreaterE
  | Equals
  deriving Show
