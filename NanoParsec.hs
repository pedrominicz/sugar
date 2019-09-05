module NanoParsec where

import Data.Char ()
import Control.Monad ()
import Control.Applicative ()

newtype Parser a = Parser { parse :: String -> [(a, String)] }

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, _)]    -> error "NanoParsec.runParser: parser did not consume entire stream"
    _           -> error "NanoParsec.runParser: parser error"

item :: Parser Char
item = Parser $ \s ->
  case s of
    []     -> []
    (c:cs) -> [(c,cs)]
