module Expr
  ( Name
  , Statement(..)
  , Expr(..)
  , Type(..)
  ) where

type Name = String

data Statement
  = Let String Expr
  | Expr Expr

data Expr
  = Ref Int
  | Global String
  | Lam Type Expr
  | App Expr Expr
  | Num Integer
  | Bool Bool

instance Show Expr where
  show x = showExpr 0 x

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

showExpr :: Int -> Expr -> String
showExpr n (Ref x)
  | (n - x) > 0 = vars !! (n - x - 1)
  | otherwise   = "_"
showExpr _ (Global x) = x
showExpr n (App x y)  = "(" ++ x' ++ " " ++ y' ++ ")"
  where x' = showExpr n x
        y' = showExpr n y
showExpr n (Lam t x)  = "(λ" ++ arg ++ ":" ++ t' ++ "." ++ x' ++ ")"
  where arg = vars !! n
        t'  = showType False t
        x'  = showExpr (n + 1) x
showExpr _ (Num x)    = show x
showExpr _ (Bool x)   = if x then "true" else "false"

showType :: Bool -> Type -> String
showType left (LamT x y)
  | left      = "(" ++ x' ++ " -> " ++ y' ++ ")"
  | otherwise =        x' ++ " -> " ++ y'
  where x' = showType True x
        y' = showType False y
showType _ NumT       = "Num"
showType _ BoolT      = "Bool"
