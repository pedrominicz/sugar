module Sugar
  ( Sugar(..)
  , Name
  ) where

import Text.Parsec
import Text.Parsec.String (Parser)

type Name = String

data Sugar
  = Var Name
  | App Sugar Sugar
  | Lam Name Sugar

instance Show Sugar where
  show (Var x)   = x
  show (App x y) = "(" ++ show x ++ " " ++ show y ++ ")"
  show (Lam x y) = "(" ++ x ++ "." ++ show y ++ ")"

instance Read Sugar where
  readsPrec _ =
    \s -> case parse (whitespace *> sugar <* eof) "" s of
      Left e  -> error $ show e
      Right x -> [(x, "")]

sugar :: Parser Sugar
sugar = lambda
    <|> application
    <|> variable
    <|> parens sugar

variable :: Parser Sugar
variable = Var <$> name

application :: Parser Sugar
application = (variable <|> parens sugar) `chainl1` application'
  where application' = pure App <* whitespace

lambda :: Parser Sugar
lambda = try $ do
  x <- name
  char '.' *> whitespace
  y <- sugar
  pure (Lam x y)

name :: Parser Name
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
