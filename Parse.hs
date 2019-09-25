module Parse
  ( parse
  ) where

import Expr
import Type

import Control.Monad.Except
import Control.Monad.Reader

import Data.List (elemIndex)
import Text.Parsec hiding (parse)

type Parser = ParsecT String () (Reader [Name])

parse :: String -> Except String Statement
parse s =
  case runReader (runParserT (whitespace *> statement <* eof) () "" s) [] of
    Left e  -> throwError $ show e
    Right x -> return $ x

isReserved :: Name -> Bool
isReserved x = elem x ["let", "in", "Num", "Bool", "true", "false"]

statement :: Parser Statement
statement = Expr <$> letExpr
        <|> letStatement
        <|> Expr <$> expression

letStatement :: Parser Statement
letStatement = try $ do
  reserved "let" ()
  x <- name
  char '=' *> whitespace
  e <- expression
  return $ Let' x e

expression :: Parser Expr
expression = letExpr
         <|> lambda
         <|> addExpr
         <|> mulExpr
         <|> compareExpr
         <|> application
         <|> boolean
         <|> variable
         <|> number
         <|> parens expression

letExpr :: Parser Expr
letExpr = try $ do
  reserved "let" ()
  x <- name
  char '=' *> whitespace
  e <- expression
  reserved "in" ()
  y <- local (x:) expression
  return $ Let e y

lambda :: Parser Expr
lambda = try $ do
  optional $ char 'λ' *> whitespace
  x <- name
  t <- maybeType
  char '.' *> whitespace
  y <- local (x:) expression
  return $ Lam t y

maybeType :: Parser (Maybe Type)
maybeType = optionMaybe $ do
  char ':' *> whitespace
  t <- lambdaType
  return t

lambdaType :: Parser Type
lambdaType = ty `chainr1` arrow
  where ty = reserved "Num" NumT
         <|> reserved "Bool" BoolT
         <|> parens lambdaType

        arrow = return LamT <* string "->" <* whitespace

compareExpr :: Parser Expr
compareExpr = try $ operator <*> expression' <*> expression'
  where expression' = application
                  <|> boolean
                  <|> variable
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
                  <|> compareExpr
                  <|> application
                  <|> boolean
                  <|> variable
                  <|> number
                  <|> parens expression

        operator = choice
          [ Op Add <$ char '+'
          , Op Sub <$ char '-'
          ] <* whitespace

mulExpr :: Parser Expr
mulExpr = try $ expression' `chainl1` operator
  where expression' = compareExpr
                  <|> application
                  <|> boolean
                  <|> variable
                  <|> number
                  <|> parens expression

        operator = choice
          [ Op Mul <$ char '*'
          , Op Div <$ char '/'
          , Op Mod <$ char '%'
          ] <* whitespace

application :: Parser Expr
application = try $ expression' `chainl1` return App
  where expression' = boolean
                  <|> variable
                  <|> number
                  <|> parens expression

boolean :: Parser Expr
boolean = Bool <$> (reserved "true" True <|> reserved "false" False)

variable :: Parser Expr
variable = do
  x   <- name
  env <- ask
  case elemIndex x env of
    Just i  -> return $ Ref i
    Nothing -> return $ Global x

number :: Parser Expr
number = do
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
