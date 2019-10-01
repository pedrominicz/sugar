module Expr
  ( Name
  , Statement(..)
  , Expr(..)
  , Op(..)
  , opType
  ) where

import Type

type Name = String

data Statement
  = Let Name Expr
  | Expr Expr
  deriving Show

data Expr
  = Var Name
  | Lam Name Expr
  | App Expr Expr
  | Num Integer
  | Bool Bool
  | Op Op Expr Expr
  | If Expr Expr Expr
  | Fix Expr
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

instance Show Op where
  show Add      = "+"
  show Sub      = "-"
  show Mul      = "*"
  show Div      = "/"
  show Mod      = "%"
  show Less     = "<"
  show LessE    = "<="
  show Greater  = ">"
  show GreaterE = ">="
  show Equals   = "=="

opType :: Op -> Type
opType Add = NumT
opType Sub = NumT
opType Mul = NumT
opType Div = NumT
opType Mod = NumT
opType _   = BoolT
