-- 1. fac (-1) would run forever.
fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n-1)

-- 2.
sumdown :: Int -> Int 
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

--3. 
pow :: Int -> Int -> Int 
_ `pow` 0 = 1
a `pow` b = a * ( a `pow` (b-1))
