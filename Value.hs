module Value
  ( Value(..)
  , Environment
  ) where

import Expr
import Type

data Value
  = Closure [Value] Expr
  | Number Integer
  | Boolean Bool

instance Show Value where
  show (Closure _ _) = "<closure>"
  show (Number x)    = show x
  show (Boolean x)   = if x then "true" else "false"

type Environment = [(String, (Scheme, Value))]
