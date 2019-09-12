module Main where

import Eval

import System.IO

repl :: IO ()
repl = do
  input <- read <$> (putStr "> " >> getLine)
  let input'   = substitute 's' (read "x.y.z.xz(yz)") input
  let input''  = substitute 'k' (read "x.y.x") input'
  let input''' = substitute 'i' (read "x.x") input''
  putStrLn $ show $ eval input'''
  repl

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  repl
