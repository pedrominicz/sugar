module Expr
  ( Statement(..)
  , Expr(..)
  , Type(..)
  ) where

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
  show x = showType 0 x

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
        t'  = showType 0 t
        x'  = showExpr (n + 1) x
showExpr _ (Num x)    = show x
showExpr _ (Bool x)   = if x then "true" else "false"

showType :: Int -> Type -> String
showType 0 (LamT x y) = showType 1 x ++ " -> " ++ showType 0 y
showType n (LamT x y) = "(" ++ x' ++ " -> " ++ y' ++ ")"
  where x' = showType (n + 1) x
        y' = showType n y
showType _ NumT       = "Num"
showType _ BoolT      = "Bool"
