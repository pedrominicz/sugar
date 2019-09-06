module Main where

import Text.Parsec
import Text.Parsec.Text

import qualified Data.Text as T

data SExp
  = Atom T.Text
  | List [SExp]
  | Number Integer
  deriving Show

parseSExp :: Parser SExp
parseSExp = parseNumber
  <|> parseAtom
  <|> parseList

parseNumber :: Parser SExp
parseNumber = Number <$> (sign <*> decimal)
  where sign = char '-' *> return negate
           <|> char '+' *> return id
           <|> return id
        decimal = read <$> many1 digit

parseAtom :: Parser SExp
parseAtom = Atom . T.pack <$> many1 (letter <|> digit)

parseList :: Parser SExp
parseList = do
  _ <- char '('
  l <- sepBy parseSExp spaces
  _ <- char ')'
  return $ List l

main :: IO ()
main = do
  input <- T.pack <$> getLine
  putStrLn $ case parse parseSExp "<stdin>" input of
    Left err -> show err
    Right x  -> show x
