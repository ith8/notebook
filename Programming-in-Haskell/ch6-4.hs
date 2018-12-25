-- Euclid's algorithm for finding GCD, greatest common divisor
euclid :: Int -> Int -> Int 
euclid x y 
  | x == y = x
  | x > y = euclid (x-y) y 
  | otherwise = euclid (y-x) x
