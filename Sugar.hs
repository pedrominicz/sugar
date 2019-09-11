module Sugar (Sugar(..), Name) where

import Text.Parsec
import Text.Parsec.String (Parser)

type Name = String

data Sugar
  = Var Name
  | App Sugar Sugar
  | Lam Name Sugar
  | Num Integer

instance Show Sugar where
  show (Var x)   = x
  show (App x y) = "(" ++ show x ++ " " ++ show y ++ ")"
  show (Lam x y) = "(\\" ++ x ++ " -> " ++ show y ++ ")"
  show (Num x)   = show x

instance Read Sugar where
  readsPrec _ =
    \s -> case parse (whitespace *> sugar <* whitespace <* eof) "" s of
      Left e  -> error $ show e
      Right x -> [(x, "")]

sugar :: Parser Sugar
sugar = variable
    <|> application
    <|> lambda
    <|> number

variable :: Parser Sugar
variable = Var . pure <$> oneOf ['a'..'z']

application :: Parser Sugar
application = parens $ do
  x <- sugar
  whitespace
  y <- sugar
  pure (App x y)

lambda :: Parser Sugar
lambda = parens $ do
  x <- oneOf ['a'..'z']
  whitespace *> char '.' *> whitespace
  y <- sugar
  pure (Lam (pure x) y)

number :: Parser Sugar
number = Num . read <$> many1 digit

parens :: Parser a -> Parser a
parens p = try $ between open close (whitespace *> p <* whitespace)
  where open  = char '('
        close = char ')' <?> "')'"

whitespace :: Parser ()
whitespace = skipMany (skipMany1 space <|> comment <?> "")
  where comment = do
          _ <- try $ char '#'
          skipMany (satisfy (/= '\n'))
          pure ()
