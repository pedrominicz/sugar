module Sugar (Sugar(..), Name) where

import Text.Parsec
import Text.Parsec.String (Parser)

type Name = Char

data Sugar
  = Var Name
  | App Sugar Sugar
  | Lam Name Sugar

instance Show Sugar where
  show (Var x)   = pure x
  show (App x y) = "(" ++ show x ++ " " ++ show y ++ ")"
  show (Lam x y) = "(" ++ pure x ++ "." ++ show y ++ ")"

instance Read Sugar where
  readsPrec _ =
    \s -> case parse (whitespace *> sugar <* whitespace <* eof) "" s of
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
  where application' = whitespace *> pure App

lambda :: Parser Sugar
lambda = try $ do
  x <- name
  whitespace *> char '.' *> whitespace
  y <- sugar
  pure (Lam x y)

name :: Parser Name
name = oneOf ['a'..'z']

parens :: Parser a -> Parser a
parens p = try $ between open close (whitespace *> p <* whitespace)
  where open  = char '('
        close = char ')' <?> "')'"

whitespace :: Parser ()
whitespace = skipMany (skipMany1 space <|> comment <?> "")
  where comment = do
          _ <- try $ char '#'
          skipMany (satisfy (/= '\n'))
