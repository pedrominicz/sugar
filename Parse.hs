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
isReserved x = elem x ["true" , "false" , "if" , "then" , "else"]

statement :: Parser Statement
statement = letStatement
        <|> Expr <$> expression

letStatement :: Parser Statement
letStatement = try $ do
  x <- name
  char '=' *> whitespace
  e <- expression
  seen <- get
  if S.member x seen
    then return $ Let x (Fix (Lam x e))
    else return $ Let x e

expression :: Parser Expr
expression = ifExpr
         <|> lambda
         <|> compareExpr
         <|> addExpr
         <|> mulExpr
         <|> application
         <|> variable
         <|> boolean
         <|> number
         <|> parens expression

ifExpr :: Parser Expr
ifExpr = try $ do
  reserved "if" ()
  cond <- expression
  reserved "then" ()
  x <- expression
  reserved "else" ()
  y <- expression
  return $ If cond x y

lambda :: Parser Expr
lambda = try $ do
  char '\\' *> whitespace
  xs <- many1 name
  old <- get
  string "->" *> whitespace
  y <- expression
  new <- get
  put $ old `S.union` (S.difference (S.fromList xs) new)
  return $ foldr (\x y -> Lam x y) y xs

compareExpr :: Parser Expr
compareExpr = try $ expression' `chainl1` operator
  where expression' = addExpr
                  <|> mulExpr
                  <|> application
                  <|> variable
                  <|> boolean
                  <|> number
                  <|> parens expression

        operator = choice
          [ Op Less     <$ char '<'
          , Op LessE    <$ try (string "<=")
          , Op Greater  <$ char '>'
          , Op GreaterE <$ try (string ">=")
          , Op Equals   <$ try (string "==")
          ] <* whitespace

addExpr :: Parser Expr
addExpr = try $ expression' `chainl1` operator
  where expression' = mulExpr
                  <|> application
                  <|> variable
                  <|> boolean
                  <|> number
                  <|> parens expression

        operator = choice
          [ Op Add <$ char '+'
          , Op Sub <$ char '-'
          ] <* whitespace

mulExpr :: Parser Expr
mulExpr = try $ expression' `chainl1` operator
  where expression' = application
                  <|> variable
                  <|> boolean
                  <|> number
                  <|> parens expression

        operator = choice
          [ Op Mul <$ char '*'
          , Op Div <$ char '/'
          , Op Mod <$ char '%'
          ] <* whitespace

application :: Parser Expr
application = try $ expression' `chainl1` return App
  where expression' = variable
                  <|> boolean
                  <|> number
                  <|> parens expression

boolean :: Parser Expr
boolean = Bool <$> (reserved "true" True <|> reserved "false" False)

variable :: Parser Expr
variable = do
  x <- name
  modify $ S.insert x
  return $ Var x

number :: Parser Expr
number = do
  sign   <- option ' ' (char '-')
  digits <- many1 digit
  whitespace
  return $ Num (read (sign:digits))

name :: Parser String
name = try $ do
  c  <- letter
  cs <- many alphaNum
  whitespace
  let s = c:cs
  if isReserved s
    then unexpected s
    else return s

reserved :: String -> a -> Parser a
reserved s x = try $ do
  string s *> notFollowedBy alphaNum *> whitespace
  return x

parens :: Parser a -> Parser a
parens p = between open close p
  where open  = char '(' <* whitespace
        close = char ')' <* whitespace

whitespace :: Parser ()
whitespace = skipMany (skipMany1 space <|> comment)
  where comment = try $ do
          _ <- char '#'
          skipMany (satisfy (/= '\n'))
