module Eval (
  eval
  ) where

import Sugar

-- <form> -> <definition> | <expression>
eval :: Context -> Sugar -> (Context, Sugar)

-- <definition> -> (let (<identifier>+) <body>)
eval ctx x@(List (Identifier "let":(List (Identifier f:args)):body)) =
  ((f, \_ -> Identifier ":)"):ctx, x)
eval _ x@(List (Identifier "let":_)) =
  error $ "; malformed definition '" ++ show x ++ "'"

-- <variable> -> <identifier>
eval ctx (Identifier x) =
  case lookup x ctx of
    Just f  -> (ctx, f [])
    Nothing -> error $ "; unbound identifier '" ++ x ++ "'"

eval ctx (List (Identifier x:args)) =
  case lookup x ctx of
    Just f  -> (ctx, f args)
    Nothing -> error $ "; unbound identifier '" ++ x ++ "'"
eval ctx (Number x) = (ctx, Number x)
eval _ _ = error "Eval.eval: undefined"

{-

<program> -> form*
<form>    -> <definition> | <expression>

<definition> -> (let (<variable>+) <body>)
<variable>   -> <identifier>
<body>       -> <expression>

<expression>  -> <constant>
               | <variable>
               | (if <expression> <expression> <expression>)
               | <application>
<constant>    -> <number>
               | <string>
               | <array>
<application> -> (<expression> <expression>*)

-}
