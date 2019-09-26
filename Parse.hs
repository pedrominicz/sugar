module Parse
  ( parse
  ) where

import Expr

import Control.Monad.Except

import Text.Parsec hiding (parse)
import Text.Parsec.String (Parser)

parse :: String -> Except String Statement
parse s =
  case runParser (whitespace *> statement <* eof) () "" s of
    Left e  -> throwError $ show e
    Right x -> return $ x

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
  return $ Let x (Fix (Lam x e))

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
  optional $ char 'λ' *> whitespace
  x <- name
  char '.' *> whitespace
  y <- expression
  return $ Lam x y

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
variable = Var <$> name

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
