-- double a digit and -9 if result >9
luhnDouble :: Int -> Int 
luhnDouble n 
  | n > 4 = 2*n-9
  | otherwise = 2*n

luhnTotal :: Int -> Int -> Int -> Int -> Int
luhnTotal x y z w = luhnDouble x + luhnDouble y + luhnDouble z + luhnDouble w

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z w
  | (luhnTotal x y z w) `mod` 10 == 0 = True
  | otherwise = False 
