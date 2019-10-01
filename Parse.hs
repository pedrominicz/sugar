module Parse
  ( parse
  ) where

import Expr

import Control.Monad.State
import Text.Parsec hiding (parse, State)
import qualified Data.Set as S

type Parser = ParsecT String () (State (S.Set Name))

parse :: String -> Either String Statement
parse s =
  case evalState (runParserT (whitespace *> statement <* eof) () "" s) S.empty of
    Left e  -> Left $ show e
    Right x -> Right x

isReserved :: Name -> Bool
isReserved x = elem x ["let" , "if" , "then" , "else", "true" , "false"]

statement :: Parser Statement
statement = letStatement
        <|> Expr <$> expression

letStatement :: Parser Statement
letStatement = do
  (x:xs) <- try $ many1 name <* char '=' <* whitespace
  y' <- expression
  seen <- get
  let y = foldr Lam y' xs
  if S.member x seen
    then return $ Let x (Fix (Lam x y))
    else return $ Let x y

expression :: Parser Expr
expression = ifExpr
         <|> lambda
         <|> compareExpr

ifExpr :: Parser Expr
ifExpr = do
  try $ reserved "if"
  cond <- expression
  reserved "then"
  x <- expression
  reserved "else"
  y <- expression
  return $ If cond x y

lambda :: Parser Expr
lambda = do
  try $ char '\\' *> whitespace
  xs <- many1 name
  old <- get
  string "->" *> whitespace
  y <- expression
  new <- get
  put $ old `S.union` (S.fromList xs `S.difference` new)
  return $ foldr (\x y -> Lam x y) y xs

compareExpr :: Parser Expr
compareExpr = addExpr `chainl1` operator
  where operator = choice
          [ Op LessE    <$ try (string "<=")
          , Op Less     <$ char '<'
          , Op GreaterE <$ try (string ">=")
          , Op Greater  <$ char '>'
          , Op Equals   <$ try (string "==")
          ] <* whitespace

addExpr :: Parser Expr
addExpr = mulExpr `chainl1` operator
  where operator = choice
          [ Op Add <$ char '+'
          , Op Sub <$ char '-'
          ] <* whitespace

mulExpr :: Parser Expr
mulExpr = application `chainl1` operator
  where operator = choice
          [ Op Mul <$ char '*'
          , Op Div <$ char '/'
          , Op Mod <$ char '%'
          ] <* whitespace

application :: Parser Expr
application = expression' `chainl1` return App
  where expression' = variable
                  <|> boolean
                  <|> number
                  <|> parens expression

variable :: Parser Expr
variable = try $ do
  x <- name
  modify $ S.insert x
  return $ Var x

boolean :: Parser Expr
boolean = try $ Bool <$>
  (reserved "true" *> return True <|> reserved "false" *> return False)

number :: Parser Expr
number = try $ do
  sign   <- option ' ' (char '-')
  digits <- many1 digit
  whitespace
  return $ Num (read (sign:digits))

name :: Parser String
name = do
  c  <- letter
  cs <- many alphaNum
  whitespace
  let s = c:cs
  if isReserved s
    then unexpected s
    else return s

reserved :: String -> Parser ()
reserved s = string s *> notFollowedBy alphaNum *> whitespace

parens :: Parser a -> Parser a
parens p = between open close p
  where open  = char '(' <* whitespace
        close = char ')' <* whitespace

whitespace :: Parser ()
whitespace = skipMany (skipMany1 space <|> comment)
  where comment = do
          _ <- try $ char '#'
          skipMany (satisfy (/= '\n'))
