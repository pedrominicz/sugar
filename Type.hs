module Type
  ( Type(..)
  , Scheme(..)
  ) where

import Control.Monad.State
import Data.List (elemIndex)

data Type
  = TVar Int
  | LamT Type Type
  | NumT
  | BoolT

data Scheme = Forall [Int] Type
  deriving Show

instance Show Type where
  show x = evalState (showType False x) []

showType :: Bool -> Type -> State [Int] String
showType left (TVar x) = do
  env <- get
  case elemIndex x env of
    Just x' -> return $ vars !! x'
    Nothing -> do
      modify (++[x])
      showType left (TVar x)
showType left (LamT x y) = do
  x' <- showType True x
  y' <- showType False y
  if left
    then return $ "(" ++ x' ++ " -> " ++ y' ++ ")"
    else return $        x' ++ " -> " ++ y'
showType _ NumT  = return $ "Num"
showType _ BoolT = return $ "Bool"

vars :: [String]
vars = [c:show' n | n <- [0..], c <- ['a'..'z']]
  where show' 0 = ""
        show' x = show x
