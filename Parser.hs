module Parser
  ( readSugar
  , readSugarFile
  ) where

import Sugar

import Text.Parsec hiding (string)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

import Control.Monad (mzero)
import Data.Functor.Identity (Identity)

lexer :: Tok.GenTokenParser String () Identity
lexer = Tok.makeTokenParser style
  where style = Tok.LanguageDef
          { Tok.commentStart    = ""
          , Tok.commentEnd      = ""
          , Tok.commentLine     = ";"
          , Tok.nestedComments  = True
          , Tok.identStart      = letter <|> oneOf "*+-./<=>?"
          , Tok.identLetter     = letter <|> oneOf "*+-./<=>?" <|> digit
          , Tok.opStart         = mzero
          , Tok.opLetter        = mzero
          , Tok.reservedOpNames = []
          , Tok.reservedNames   = []
          , Tok.caseSensitive   = True
          }

sugarList :: Parser [Sugar]
sugarList = sugar `sepBy` Tok.whiteSpace lexer

sugar :: Parser Sugar
sugar = array
    <|> number
    <|> identifier
    <|> list
    <|> string

array :: Parser Sugar
array = Array <$> Tok.brackets lexer sugarList

number :: Parser Sugar
number = Number <$> try (sign <*> Tok.decimal lexer) <* Tok.whiteSpace lexer
  where sign = char '-' *> return negate
           <|> char '+' *> return id
           <|> return id

identifier :: Parser Sugar
identifier = Identifier <$> Tok.identifier lexer

list :: Parser Sugar
list = List <$> Tok.parens lexer sugarList

string :: Parser Sugar
string = String <$> Tok.stringLiteral lexer

wrap :: ParsecT String () Identity a -> ParsecT String () Identity a
wrap p = Tok.whiteSpace lexer *> p <* Tok.whiteSpace lexer <* eof

readSugar :: String -> Sugar
readSugar s =
  case parse (wrap sugar) "<stdin>" s of
    Left _  -> error "; parse error"
    Right x -> x

readSugarFile :: SourceName -> String -> Sugar
readSugarFile name s =
  case parse (wrap $ List <$> sugarList) name s of
    Left _  -> error "; parse error"
    Right x -> x
