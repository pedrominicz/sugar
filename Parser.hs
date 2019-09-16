module Parser
  ( parseExpr
  ) where

import Expr

import Control.Monad.Reader
import Data.List
import Text.Parsec

type Parser = ParsecT String () (Reader [String])

parseExpr :: String -> Expr
parseExpr s =
  case runReader (runParserT (whitespace *> expression <* eof) () "" s) [] of
    Left e  -> error $ show e
    Right x -> x

expression :: Parser Expr
expression = lambda
         <|> application
         <|> boolean
         <|> variable
         <|> number
         <|> parens expression

lambda :: Parser Expr
lambda = try $ do
  x <- name
  char '.' *> whitespace
  y <- local (x:) expression
  pure (Lam NumT y)

application :: Parser Expr
application = expression' `chainl1` pure App
  where expression' = boolean
                  <|> variable
                  <|> number
                  <|> parens expression

boolean :: Parser Expr
boolean = try $ do
  x <- name
  case x of
    "true"  -> pure $ Bool True
    "false" -> pure $ Bool False
    _       -> unexpected x

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

parens :: Parser a -> Parser a
parens p = between open close p
  where open  = char '(' <* whitespace
        close = char ')' <* whitespace

whitespace :: Parser ()
whitespace = skipMany (skipMany1 space <|> comment)
  where comment = try $ do
          _ <- char '#'
          skipMany (satisfy (/= '\n'))
