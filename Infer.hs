module Infer
  ( infer
  ) where

import Expr
import Type
import Value

import Control.Monad.Except
import Control.Monad.State

import Safe (atMay)

type Binding = (Int, [(Int, Type)])

type Infer = StateT Binding (StateT Environment (Except String))

infer :: Expr -> StateT Environment (Except String) Scheme
infer expr = flip evalStateT (0, []) $ do
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
    Nothing -> error $ "Infer.infer': unbound reference: " ++ show x

infer' _ (Global x) = do
  env <- lift $ get
  case lookup x env of
    Just (t, _) -> instantiate t
    Nothing     -> throwError $ "unbound variable: " ++ x

infer' env (Lam (Just t) x) = do
  tx <- infer' (Forall [] t:env) x
  return $ LamT t tx

infer' env (Lam Nothing x) = do
  t  <- newType
  tx <- infer' (Forall [] t:env) x
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
  return $ case op of
    Add -> NumT
    Sub -> NumT
    Mul -> NumT
    Div -> NumT
    Mod -> NumT
    _   -> BoolT

instantiate :: Scheme -> Infer Type
instantiate (Forall xs x) = do
  xs' <- mapM (\x' -> (,) x' <$> newType) xs
  return $ instantiate' xs' x

instantiate' :: [(Int, Type)] -> Type -> Type
instantiate' xs t@(TVar x) =
  case lookup x xs of
    Just x' -> x'
    Nothing -> t
instantiate' xs (LamT x y) = LamT (instantiate' xs x) (instantiate' xs y)
instantiate' _ x           = x

generalize :: [Scheme] -> Type -> Scheme
generalize env x = Forall (filter (`notElem` freeEnv) freeVar) x
  where freeEnv = concatMap (\(Forall _ x') -> free x') env
        freeVar = free x

free :: Type -> [Int]
free (TVar x)   = [x]
free (LamT x y) = free x ++ free y
free _          = []

unify :: Type -> Type -> Infer ()
unify x y = do
  x' <- applyBindings x
  y' <- applyBindings y
  unify' x' y'

unify' :: Type -> Type -> Infer ()
unify' (TVar x) y = do
  y' <- applyBindings y
  modify (\(i, env) -> (i, (x, y'):env))
unify' x y@(TVar _) = unify' y x
unify' (LamT x x') (LamT y y') = do
  unify' x y
  x'' <- applyBindings x'
  y'' <- applyBindings y'
  unify' x'' y''
unify' NumT NumT   = return ()
unify' BoolT BoolT = return ()
unify' _ _         = throwError "cannot match types"

applyBindings :: Type -> Infer Type
applyBindings (TVar x) = do
  (_, env) <- get
  case lookup x env of
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
occursGuard _ _                   = return ()
