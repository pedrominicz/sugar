module Main where

import Text.Parsec
import Text.Parsec.Text

import qualified Data.Text as T

data SExp
  = Atom T.Text
  | List [SExp]
  deriving Show

parseSExp :: Parser SExp
parseSExp = parseAtom
  <|> parseList

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
