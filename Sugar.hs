module Sugar where

type Context = [(String, [Sugar] -> Sugar)]

data Sugar
  = Array [Sugar]
  | Identifier String
  | List [Sugar]
  | Number Integer
  | String String

instance Show Sugar where
  show (Array xs)     = "[" ++ (unwords $ map show xs) ++ "]"
  show (Identifier x) = x
  show (List xs)      = "(" ++ (unwords $ map show xs) ++ ")"
  show (Number x)     = show x
  show (String x)     = x
