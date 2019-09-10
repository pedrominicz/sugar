module Parser (Tree) where

import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.String (Parser)

data Tree
  = Atom String
  | List [Tree]
  | Number Integer
  deriving Show

instance Read Tree where
  readsPrec _ =
    \s -> case parse (wrap tree) "" s of
      Left e  -> error $ show e
      Right x -> [(x, "")]

tree :: Parser Tree
tree = number
   <|> atom
   <|> list

number :: Parser Tree
number = Number <$> try number' <* whitespace <?> "number"
  where number' = do
          sign   <- many (oneOf "+-")
          digits <- many1 digit
          return $ read (sign ++ digits)

whitespace :: Parser ()
whitespace = skipMany (skipMany1 space <|> comment <?> "")
  where comment = do
          _ <- try $ char ';'
          skipMany (satisfy (/= '\n'))
          return ()

atom :: Parser Tree
atom = Atom <$> atom' <* whitespace <?> "atom"
  where atom' = try $ do
          c  <-        letter <|> oneOf "*+-./<=>?"
          cs <- many $ letter <|> oneOf "*+-./<=>?" <|> digit
          return (c:cs)

list :: Parser Tree
list = List <$> parens (tree `sepBy` whitespace) <?> "list"

parens :: Parser a -> Parser a
parens p = between open close p
  where open  = char '(' <* whitespace
        close = char ')' <* whitespace <?> "')'"

wrap :: ParsecT String () Identity a -> ParsecT String () Identity a
wrap p = whitespace *> p <* whitespace <* eof
