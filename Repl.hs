{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Repl
  ( Repl(..)
  , runRepl
  , readInput
  , showStrLn
  , quit
  ) where

import Value

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe

import Data.Char (isSpace)
import qualified Data.Map as M

import System.Console.Haskeline

newtype Repl a = Repl { unRepl :: MaybeT (StateT Environment (InputT IO)) a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadState Environment)

instance MonadReader Environment Repl where
  ask = get
  local f x = do
    old <- get
    modify f
    x' <- x
    put old
    return x'

runRepl :: Repl () -> InputT IO ()
runRepl x = do
  _ <- evalStateT (runMaybeT (unRepl x)) M.empty
  return ()

readInput :: String -> Repl String
readInput prompt = do
  maybeInput <- Repl . lift . lift $ getInputLine prompt
  case dropWhile isSpace <$> maybeInput of
    Nothing    -> quit
    Just ""    -> readInput prompt
    Just input -> return input

showStrLn :: String -> Repl ()
showStrLn = Repl . lift . lift . outputStrLn

quit :: Repl a
quit = Repl mzero
