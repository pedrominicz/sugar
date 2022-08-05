module Type
  ( Type(..)
  , Scheme(..)
  , showType
  ) where

import Control.Monad.State

import Data.List (elemIndex)
import qualified Data.IntSet as IS

data Type
  = TVar Int
  | LamT Type Type
  | NumT
  | BoolT

instance Show Type where
  show x = evalState (showType x) []

data Scheme = Forall IS.IntSet Type
  deriving Show

-- Only used in `Test.hs`.
instance Eq Scheme where
  Forall _ t1 == Forall _ t2 = show t1 == show t2

showType :: Type -> State [Int] String
showType = showType' False

showType' :: Bool -> Type -> State [Int] String
showType' left (TVar x) = do
  env <- get
  case elemIndex x env of
    Just x' -> return $ vars !! x'
    Nothing -> do
      modify (++[x])
      showType' left (TVar x)
showType' left (LamT x y) = do
  x' <- showType' True x
  y' <- showType' False y
  if left
    then return $ "(" ++ x' ++ " -> " ++ y' ++ ")"
    else return $        x' ++ " -> " ++ y'
showType' _ NumT  = return $ "Num"
showType' _ BoolT = return $ "Bool"

vars :: [String]
vars = [c:show' n | n <- [0..], c <- ['a'..'z']]
  where show' 0 = ""
        show' x = show x
