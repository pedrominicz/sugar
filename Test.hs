module Test where

import Infer
import Parse
import Type

import Control.Monad.State
import System.IO
import Text.Parsec hiding (State)
import qualified Data.IntSet as IS

type Parser = ParsecT String () (State [(String, Int)])

parseScheme :: String -> Scheme
parseScheme s =
  case evalState (runParserT (whitespace *> parseType <* eof) () "" s) [] of
    Right x -> Forall IS.empty x
    _ -> undefined

parseType :: Parser Type
parseType = arrow <|> variable

-- Only parses arrows surrounded by parentheses.
arrow :: Parser Type
arrow = parens $ do
  t1 <- parseType
  operator "->"
  t2 <- parseType
  return $ LamT t1 t2

variable :: Parser Type
variable = do
  x <- name
  seen <- get
  case lookup x seen of
    Just x -> return (TVar x)
    Nothing -> do
      let x' = length seen
      modify $ (++ [(x, x')])
      return (TVar x')

main :: IO ()
main = do
  eof <- isEOF
  if eof
    then return ()
    else do
      putChar '.'
      t1 <- inferType . parseExpr <$> getLine
      t2 <- parseScheme <$> getLine
      if t1 /= t2 then print t1 >> print t2 else main
