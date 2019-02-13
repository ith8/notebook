-- a) Tail-recursive definitions
prod'' :: Int -> Int -> Int
prod'' 0 r = r 
prod'' n r = prod'' (n - 1) (r*n)

prod' :: Int -> Int 
prod' n = prod'' n 1

sum'' :: Int -> Int -> Int
sum'' 0 r = r
sum'' n r = sum'' (n-1) (r+n)

sum' :: Int -> Int 
sum' n = sum'' n 0

-- b) Burstall-Darlington transformation of prodsum
prodsum :: Int -> Int
prodsum x = a + b 
  where (a,b) = g x

g :: Int -> (Int,Int) 
g 0 = (1,0)
g x = (x*a,x+b)
  where (a,b) = g (x-1)

-- c) tail-recursive prodsum
prodsum'' :: Int -> Int -> Int -> Int
prodsum'' 0 p s = p + s
prodsum'' n p s = prodsum'' (n-1) (p*n) (s+n)

prodsum' :: Int -> Int
prodsum' n = prodsum'' n 1 0
