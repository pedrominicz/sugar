module Parse
  ( parse
  ) where

import Expr

import Control.Monad.State

import Data.Maybe
import qualified Data.Set as S

import Text.Parsec hiding (parse, State)

type Parser = ParsecT String () (State (S.Set Name))

parse :: String -> Either String Statement
parse s =
  case evalState (runParserT (whitespace *> statement <* eof) () "" s) S.empty of
    Left e  -> Left $ show e
    Right x -> Right x

isReserved :: Name -> Bool
isReserved x = elem x ["if", "then", "else", "true", "false"]

statement :: Parser Statement
statement = letStatement
        <|> Expr <$> expression

letStatement :: Parser Statement
letStatement = do
  (x, args) <- try $ do
    x    <- name
    args <- many argument
    operator "="
    return (x, args)
  y' <- expression
  seen <- get
  let y = foldr Lam y' args
  if S.member x seen
    then return $ Let x (Fix (Lam (Just x) y))
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
  try $ operator "\\"
  xs <- many1 argument
  operator "->"
  old <- get
  y <- expression
  new <- get
  put $ old `S.union` (S.fromList (catMaybes xs) `S.difference` new)
  return $ foldr (\x y -> Lam x y) y xs

compareExpr :: Parser Expr
compareExpr = addExpr `chainl1` operator'
  where operator' = choice
          [ Op LessE    <$ try (operator "<=")
          , Op Less     <$ operator "<"
          , Op GreaterE <$ try (operator ">=")
          , Op Greater  <$ operator ">"
          , Op Equals   <$ try (operator "==")
          ]

addExpr :: Parser Expr
addExpr = mulExpr `chainl1` operator'
  where operator' = choice
          [ Op Add <$ operator "+"
          , Op Sub <$ operator "-"
          ]

mulExpr :: Parser Expr
mulExpr = application `chainl1` operator'
  where operator' = choice
          [ Op Mul <$ operator "*"
          , Op Div <$ operator "/"
          , Op Mod <$ operator "%"
          ]

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

argument :: Parser (Maybe String)
argument = nothing <|> Just <$> name
  where nothing = do
          try $ operator "_"
          return Nothing

reserved :: String -> Parser ()
reserved s = string s *> notFollowedBy alphaNum *> whitespace

operator :: String -> Parser ()
operator s = string s *> notFollowedBy (oneOf "%*+-/<=>") *> whitespace

parens :: Parser a -> Parser a
parens p = between (operator "(") (operator ")") p

whitespace :: Parser ()
whitespace = skipMany (skipMany1 space <|> comment)
  where comment = do
          try $ operator "--"
          skipMany (satisfy (/= '\n'))
