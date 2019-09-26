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

import Safe (atMay)

type Binding = (Int, IM.IntMap Type)

type Infer = StateT Binding (ExceptT String Repl)

infer :: Expr -> ExceptT String Repl Scheme
infer expr = flip evalStateT (0, IM.empty) $ do
  t  <- infer' [] expr
  t' <- applyBindings t
  return $ generalize [] t'

newType :: Infer Type
newType = do
  (i, env) <- get
  put (i + 1, env)
  return $ TVar i

infer' :: [Scheme] -> Expr -> Infer Type
infer' env (Ref x) =
  case atMay env x of
    Just t  -> instantiate t
    Nothing -> error $ "Infer.infer': unbound reference"
infer' _ (Global x) = do
  env <- ask
  case M.lookup x env of
    Just (_, t) -> instantiate t
    Nothing     -> throwError $ "unbound variable: " ++ x
infer' env (Lam (Just t) x) = do
  tx <- infer' (Forall IS.empty t:env) x
  return $ LamT t tx
infer' env (Lam Nothing x) = do
  t  <- newType
  tx <- infer' (Forall IS.empty t:env) x
  return $ LamT t tx
infer' env (App x y) = do
  tx <- infer' env x
  ty <- infer' env y
  t  <- newType
  tx `unify` LamT ty t
  return t
infer' env (Let x y) = do
  t <- generalize env <$> infer' env x
  infer' (t:env) y
infer' _ (Num _)  = return NumT
infer' _ (Bool _) = return BoolT
infer' env (Op op x y) = do
  tx <- infer' env x
  ty <- infer' env y
  tx `unify` NumT
  ty `unify` NumT
  return $ opType op
infer' env (If cond x y) = do
  tcond <- infer' env cond
  tcond `unify` BoolT
  tx <- infer' env x
  ty <- infer' env y
  tx `unify` ty
  return tx
infer' env (Fix x) = do
  tx <- infer' env x
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

generalize :: [Scheme] -> Type -> Scheme
generalize env x = Forall (IS.filter (`IS.notMember` freeEnv) freeVar) x
  where freeEnv = foldr filter' IS.empty env
        freeVar = free x

        filter' (Forall _ x') env' = env' `IS.union` free x'

free :: Type -> IS.IntSet
free (TVar x)   = IS.singleton x
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
