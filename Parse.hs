module Parse
  ( parse
  ) where

import Expr

import Control.Monad.Reader
import Data.List (elemIndex)
import Text.Parsec hiding (parse)

type Parser = ParsecT String () (Reader [String])

parse :: String -> Maybe Statement
parse s =
  case runReader (runParserT (whitespace *> statement <* eof) () "" s) [] of
    Left _  -> Nothing
    Right x -> Just x

statement :: Parser Statement
statement = letExpr
        <|> Expr <$> expression

letExpr :: Parser Statement
letExpr = try $ do
  x <- name
  char '=' *> whitespace
  expr <- expression
  pure $ Let x expr

expression :: Parser Expr
expression = lambda
         <|> application
         <|> boolean
         <|> variable
         <|> number
         <|> parens expression

lambda :: Parser Expr
lambda = try $ do
  optional $ char 'λ' *> whitespace
  x <- name
  char ':' *> whitespace
  t <- lambdaType
  char '.' *> whitespace
  y <- local (x:) expression
  pure (Lam t y)

lambdaType :: Parser Type
lambdaType = ty `chainr1` arrow
  where ty = reserved "Num" NumT
         <|> reserved "Bool" BoolT
         <|> parens lambdaType

        arrow = pure LamT <* string "->" <* whitespace

application :: Parser Expr
application = expression' `chainl1` pure App
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
    Just i  -> pure $ Ref i
    Nothing -> pure $ Global x

number :: Parser Expr
number = Num <$> do
  sign   <- option ' ' (char '-')
  digits <- many1 digit
  whitespace
  pure $ read (sign:digits)

name :: Parser String
name = do
  c  <- letter
  cs <- many alphaNum
  whitespace
  pure (c:cs)

reserved :: String -> a -> Parser a
reserved s x = try $ do
  s' <- name
  if s == s'
    then pure x
    else unexpected s'

parens :: Parser a -> Parser a
parens p = between open close p
  where open  = char '(' <* whitespace
        close = char ')' <* whitespace

whitespace :: Parser ()
whitespace = skipMany (skipMany1 space <|> comment)
  where comment = try $ do
          _ <- char '#'
          skipMany (satisfy (/= '\n'))
