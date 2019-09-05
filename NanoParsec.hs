module NanoParsec where

import Data.Char
import Control.Monad
import Control.Applicative

{-
  `Parser a` is a type.
  `Parser { parse :: String -> [(a, String)] }` is its single constructor.
  `parse :: Parser a -> String -> [(a, String)]` is the actual type of `parse`.
-}
newtype Parser a = Parser { parse :: String -> [(a, String)] }

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(x, "")] -> x
    [(_, _)]  -> error "NanoParsec.runParser: parser did not consume entire stream"
    _         -> error "NanoParsec.runParser: parser error"

item :: Parser Char
item = Parser $ \s ->
  case s of
    []     -> []
    (c:cs) -> [(c,cs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(x, s') -> parse (f x) s') $ parse p s

unit :: a -> Parser a
unit x = Parser $ \s -> [(x, s)]

instance Functor Parser where
  fmap f p = Parser $ \s -> [(f x, s') | (x, s') <- parse p s]

instance Applicative Parser where
  pure      = unit
  p1 <*> p2 = Parser $ \s ->
    [(f x, s'') | (f, s') <- parse p1 s, (x, s'') <- parse p2 s']

instance Monad Parser where
  return = unit
  (>>=)  = bind

failure :: Parser a
failure = Parser $ \_ -> []

option :: Parser a -> Parser a -> Parser a
option p1 p2 = Parser $ \s ->
  case parse p1 s of
    [] -> parse p2 s
    x  -> x

combine :: Parser a -> Parser a -> Parser a
combine p1 p2 = Parser $ \s -> parse p1 s ++ parse p2 s

instance Alternative Parser where
  empty = failure
  (<|>) = option

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = item >>= \c ->
  if f c
    then unit c
    else failure

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

digit :: Parser Char
digit = satisfy isDigit

natural :: Parser Integer
natural = read <$> some digit

char :: Char -> Parser Char
char c = satisfy (c ==)

string :: String -> Parser String
string ""     = return ""
string (c:cs) = do
  _ <- char c
  _ <- string cs
  return (c:cs)

token :: Parser a -> Parser a
token p = do
  x <- p
  _ <- spaces
  return x

reserved :: String -> Parser String
reserved s = token (string s)

number :: Parser Int
number = do
  s <- string "-" <|> return ""
  ns <- some digit
  return $ read (s ++ ns)

parens :: Parser a -> Parser a
parens x = do
  _ <- reserved "("
  y <- x
  _ <- reserved ")"
  return y
