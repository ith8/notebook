-- 4.
> length [e | ns' <- choices [1,3,7,10,25,50], e <- exprs ns']
33665406

-- original valid
> length [e | ns' <- choices [1,3,7,10,25,50], e <- exprs ns', eval e /= []]
4672540

-- efficient valid
> length [e | ns' <- choices [1,3,7,10,25,50], e <- exprs ns', eval e /= []]
245644

-- 5. Generalized valid
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x /= y
valid Mul _ _ = True
valid Div x y = y /= 0 && abs(x) `mod` abs(y) == 0

> length [e | ns' <- choices [1,3,7,10,25,50], e <- exprs ns', eval e /= []]
10839369

