module Sugar where

type Context = [(String, [Sugar] -> Sugar)]

data Sugar
  = Atom String
  | List [Sugar]
  | Number Integer

instance Show Sugar where
  show (Atom x)   = x
  show (List xs)  = "(" ++ unwords (map show xs) ++ ")"
  show (Number x) = show x
