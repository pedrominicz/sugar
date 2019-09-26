```
> s x y z = x z (y z)
s : (a -> b -> c) -> (a -> b) -> a -> c
> k x y = x
k : a -> b -> a
> i = s k k
i : a -> a
> fac x = if x < 1 then 1 else x * fac (x - 1)
fac : Num -> Num
> fac 5
120
```

### Useful Resources

https://github.com/goldfirere/glambda/blob/master/src/Language/Glambda/Lex.hs

https://github.com/goldfirere/glambda/blob/master/src/Language/Glambda/Parse.hs

https://gist.github.com/pedrominicz/6867c298608a96e6db4dedd798f49e60
