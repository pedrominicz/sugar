module Expr
  ( Expr(..)
  , Type(..)
  ) where

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
vars = [[c] | c <- cs] ++ [c:show n | n <- [1..], c <- cs]
  where cs = ['a'..'z']

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
showExpr _ (Bool x)   = show x

showType :: Int -> Type -> String
showType 0 (LamT x y) = showType 1 x ++ " -> " ++ showType 0 y
showType n (LamT x y) = "(" ++ x' ++ " -> " ++ y' ++ ")"
  where x' = showType (n + 1) x
        y' = showType n y
showType _ NumT       = "Num"
showType _ BoolT      = "Bool"
