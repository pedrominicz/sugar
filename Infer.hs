module Infer
  ( infer
  ) where

import Expr
import Repl
import Type

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.Map as M

type Binding = (Int, IM.IntMap Type)

type Infer = StateT Binding (ReaderT (M.Map Name Scheme) (Except String))

infer :: Expr -> Repl (Either String Scheme)
infer expr = do
  env <- M.map snd <$> ask
  return $ runExcept $ flip runReaderT env $ flip evalStateT (0, IM.empty) $ do
    t  <- infer' expr
    t' <- applyBindings t
    return $ generalize t'

newType :: Infer Type
newType = do
  (i, env) <- get
  put (i + 1, env)
  return $ TVar i

infer' :: Expr -> Infer Type
infer' (Var x) = do
  env <- ask
  case M.lookup x env of
    Just x  -> instantiate x
    Nothing -> throwError $ "unbound variable: " ++ x
infer' (Lam x y) = do
  t  <- newType
  ty <- local (M.insert x (Forall IS.empty t)) $ infer' y
  return $ LamT t ty
infer' (App x y) = do
  tx <- infer' x
  ty <- infer' y
  t  <- newType
  tx `unify` LamT ty t
  return t
infer' (Num _)  = return NumT
infer' (Bool _) = return BoolT
infer' (Op op x y) = do
  tx <- infer' x
  ty <- infer' y
  tx `unify` NumT
  ty `unify` NumT
  return $ opType op
infer' (If cond x y) = do
  tcond <- infer' cond
  tcond `unify` BoolT
  tx <- infer' x
  ty <- infer' y
  tx `unify` ty
  return tx
infer' (Fix x) = do
  tx <- infer' x
  case tx of
    LamT t t' -> do
      t `unify` t'
      return t
    _ -> throwError $ "cannot fix: " ++ show tx

instantiate :: Scheme -> Infer Type
instantiate (Forall xs x) = do
  xs' <- IM.fromList <$> mapM (\x' -> (,) x' <$> newType) (IS.toList xs)
  return $ instantiate' xs' x

instantiate' :: IM.IntMap Type -> Type -> Type
instantiate' xs t@(TVar x) =
  case IM.lookup x xs of
    Just x' -> x'
    Nothing -> t
instantiate' xs (LamT x y) = LamT (instantiate' xs x) (instantiate' xs y)
instantiate' _ x           = x

generalize :: Type -> Scheme
generalize x = Forall (free x) x
  where free (TVar x)   = IS.singleton x
        free (LamT x y) = free x `IS.union` free y
        free _          = IS.empty

unify :: Type -> Type -> Infer ()
unify x y = do
  x' <- applyBindings x
  y' <- applyBindings y
  unify' x' y'

unify' :: Type -> Type -> Infer ()
unify' (TVar x) y = do
  y' <- applyBindings y
  modify $ \(i, env) -> (i, IM.insert x y' env)
unify' x y@(TVar _) = unify' y x
unify' (LamT x x') (LamT y y') = do
  unify' x y
  x'' <- applyBindings x'
  y'' <- applyBindings y'
  unify' x'' y''
unify' NumT NumT   = return ()
unify' BoolT BoolT = return ()
unify' _ _ = throwError "cannot match types"

applyBindings :: Type -> Infer Type
applyBindings (TVar x) = do
  (_, env) <- get
  case IM.lookup x env of
    Just x' -> do
      occursGuard x x'
      applyBindings x'
    Nothing -> return $ TVar x
applyBindings (LamT x y) = do
  x' <- applyBindings x
  y' <- applyBindings y
  return $ LamT x' y'
applyBindings NumT  = return NumT
applyBindings BoolT = return BoolT

occursGuard :: Int -> Type -> Infer ()
occursGuard x (LamT x' y') = do
  occursGuard x x'
  occursGuard x y'
occursGuard x (TVar x') | x == x' = throwError "infinite type"
occursGuard _ _ = return ()
