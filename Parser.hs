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
         <|> variable
         <|> parens expression

variable :: Parser Expr
variable = do
  x   <- name
  env <- ask
  case elemIndex x env of
    Just x' -> pure $ Ref x'
    Nothing -> pure $ Global x

application :: Parser Expr
application = (variable <|> parens expression) `chainl1` pure App

lambda :: Parser Expr
lambda = try $ do
  x <- name
  char '.' *> whitespace
  y <- local (x:) expression
  pure (Lam NumT y)

name :: Parser String
name = try $ do
  c  <- letter
  cs <- many alphaNum
  whitespace
  pure (c:cs)

parens :: Parser a -> Parser a
parens p = try $ between open close p
  where open  = char '(' <* whitespace
        close = char ')' <* whitespace

whitespace :: Parser ()
whitespace = skipMany (skipMany1 space <|> comment)
  where comment = do
          _ <- try $ char '#'
          skipMany (satisfy (/= '\n'))
