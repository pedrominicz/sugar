module Main where

import Eval

repl :: IO ()
repl = do
  input <- read <$> (putStr "> " >> getLine)
  let input'   = substitute 's' (read "f.g.x.(fx)(gx)") input
  let input''  = substitute 'k' (read "x.y.x") input'
  let input''' = substitute 'i' (read "x.x") input''
  putStrLn $ show $ eval input'''
  repl

main :: IO ()
main = repl
