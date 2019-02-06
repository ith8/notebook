fact :: Int -> Int
fact n 
  | n < 0 = -1
  | n == 0 = 1
  | otherwise = n * fact (n-1)
