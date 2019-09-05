import NanoParsec

import Control.Applicative
import Control.Monad
import System.IO

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Lit Int
  deriving Show

eval :: Expr -> Int
eval e = case e of
  Add x y -> eval x + eval y
  Sub x y -> eval x - eval y
  Mul x y -> eval x * eval y
  Lit x   -> x

int :: Parser Expr
int = do
  n <- token number
  return (Lit n)

expr :: Parser Expr
expr = term `chain` addop

term :: Parser Expr
term = factor `chain` mulop

factor :: Parser Expr
factor = int <|> parens expr

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

addop :: Parser (Expr -> Expr -> Expr)
addop = (infixOp "+" Add) <|> (infixOp "-" Sub)

mulop :: Parser (Expr -> Expr -> Expr)
mulop = infixOp "*" Mul

run :: String -> Expr
run = runParser expr

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  forever $ do
    putStr "> "
    x <- getLine
    print $ eval $ run x
