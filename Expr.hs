module Expr
  ( Name
  , Expr(..)
  , Statement(..)
  , Type(..)
  ) where

type Name = String

data Statement
  = Let' Name Expr
  | Expr Expr

data Expr
  = Ref Int
  | Global Name
  | Lam (Maybe Type) Expr
  | App Expr Expr
  | Num Integer
  | Bool Bool
  | Let Expr Expr
  deriving Show

data Type
  = LamT Type Type
  | NumT
  | BoolT
  deriving Eq

instance Show Type where
  show x = showType False x

vars :: [String]
vars = [c:show' n | n <- [0..], c <- ['a'..'z']]
  where show' 0 = ""
        show' x = show x

showType :: Bool -> Type -> String
showType left (LamT x y)
  | left      = "(" ++ x' ++ " -> " ++ y' ++ ")"
  | otherwise =        x' ++ " -> " ++ y'
  where x' = showType True x
        y' = showType False y
showType _ NumT       = "Num"
showType _ BoolT      = "Bool"
