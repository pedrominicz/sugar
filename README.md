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
> fac 100
93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
```

Example based on "Linear lambda calculus and PTIME-completeness" (Harry G. Mairson, 2004). The paper can be found [here][1]. In the paper, it is shown that type inference can be used to solve boolean circuits. Below, the function `circuit` applied to the arguments `False`, `True`, `True`, `True`, `True`, and `False` has type `a -> b -> a`, that is, the output of the circuit is `True`.

```
> True x y = x
True : a -> b -> a
> False x y = y
False : a -> b -> b
> Not p = p False True
Not : ((a -> b -> b) -> (c -> d -> c) -> e) -> e
> And p q = p q False
And : (a -> (b -> c -> c) -> d) -> a -> d
> Or p q = p True q
Or : ((a -> b -> a) -> c -> d) -> c -> d
> Or False True
a -> b -> a
> And True False
a -> b -> b
> Not True
a -> b -> b
> Same p = p True False
Same : ((a -> b -> a) -> (c -> d -> d) -> e) -> e
> Same True
a -> b -> a
> Same False
a -> b -> b
> K x y = x
K : a -> b -> a
> Weird p = K (Same p) (Not p)
Weird : ((a -> a -> a) -> (b -> b -> b) -> c) -> c
> Weird True
a -> a -> a
> Weird False
a -> a -> a
> Pair x y z = z x y
Pair : a -> b -> (a -> b -> c) -> c
> Copy p = p (Pair True True) (Pair False False)
Copy : ((((a -> b -> a) -> (c -> d -> c) -> e) -> e) -> (((f -> g -> g) -> (h -> i -> i) -> j) -> j) -> k) -> k
> Copy True
((a -> b -> a) -> (c -> d -> c) -> e) -> e
> Copy False
((a -> b -> b) -> (c -> d -> d) -> e) -> e
> Unweird p = (Copy p) (\p1 p2 -> K (Same p1) (Not p2))
Unweird : ((((a -> b -> a) -> (c -> d -> c) -> e) -> e) -> (((f -> g -> g) -> (h -> i -> i) -> j) -> j) -> (((k -> l -> k) -> (m -> n -> n) -> o) -> ((p -> q -> q) -> (r -> s -> r) -> t) -> o) -> u) -> u
> Unweird True
a -> b -> a
> Unweird False
a -> b -> b
> circuit e1 e2 e3 e4 e5 e6 = (\e7 -> (\e8 -> (Copy (And e7 e8)) (\e9 e10 -> (\e11 -> (\e12 -> Or e11 e12) (Or e10 e6)) (Or e1 e9))) (And e4 e5)) (And e2 e3)
circuit : ((a -> b -> a) -> c -> (d -> e -> d) -> f -> g) -> (h -> (i -> j -> j) -> k -> (l -> m -> m) -> (((n -> o -> n) -> (p -> q -> p) -> r) -> r) -> (((s -> t -> t) -> (u -> v -> v) -> w) -> w) -> (c -> ((x -> y -> x) -> z -> f) -> g) -> a1) -> h -> (b1 -> (c1 -> d1 -> d1) -> k) -> b1 -> z -> a1
> circuit False True True True True False
a -> b -> a
```

### Useful Resources

https://arxiv.org/pdf/1608.03912.pdf

https://gist.github.com/pedrominicz/475b110a30f32fb4fc0b338654bbbcc1

[1]: https://www.cs.brandeis.edu/~mairson/Papers/jfp02.pdf
