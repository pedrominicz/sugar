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
          , Tok.nestedComments  = False
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
sugar = number
    <|> identifier
    <|> list
    <|> string

number :: Parser Sugar
number = Number <$> try (sign <*> Tok.decimal lexer)
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

readSugar :: String -> Either ParseError Sugar
readSugar = parse (wrap sugar) "<stdin>"

readSugarFile :: SourceName -> String -> Either ParseError Sugar
readSugarFile = parse $ wrap (List <$> sugarList)
