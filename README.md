```
> and x y = if x then y else false
and : Bool -> Bool -> Bool
> ack m n = if m == 0 then n + 1 else if and (m > 0) (n == 0) then ack (m - 1) 1 else ack (m - 1) (ack m (n - 1))
ack : Num -> Num -> Num
> ack 3 5
253
```

### Useful Resources

https://github.com/goldfirere/glambda/blob/master/src/Language/Glambda/Lex.hs

https://github.com/goldfirere/glambda/blob/master/src/Language/Glambda/Parse.hs

https://gist.github.com/pedrominicz/6867c298608a96e6db4dedd798f49e60
