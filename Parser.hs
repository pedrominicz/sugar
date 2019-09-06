module Parser
  ( readSugar
  , readSugarFile
  ) where

import Sugar

import Text.Parsec hiding (string)
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok

import Control.Monad (mzero)
import Data.Functor.Identity (Identity)
import qualified Data.Text as T

lexer :: Tok.GenTokenParser T.Text () Identity
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
identifier = Identifier . T.pack <$> Tok.identifier lexer

list :: Parser Sugar
list = List <$> Tok.parens lexer sugarList

string :: Parser Sugar
string = String . T.pack <$> Tok.stringLiteral lexer

wrap :: ParsecT T.Text () Identity a -> ParsecT T.Text () Identity a
wrap p = Tok.whiteSpace lexer *> p <* Tok.whiteSpace lexer <* eof

readSugar :: T.Text -> Either ParseError Sugar
readSugar = parse (wrap sugar) "<stdin>"

readSugarFile :: SourceName -> T.Text -> Either ParseError Sugar
readSugarFile = parse $ wrap (List <$> sugarList)
