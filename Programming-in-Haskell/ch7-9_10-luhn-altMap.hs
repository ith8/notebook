-- 9. Alternating mapping
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : altMap g f xs

-- 10. Luhn algorithm
-- double a digit and -9 if result >9
luhnDouble :: Int -> Int 
luhnDouble n 
  | n > 4 = 2*n-9
  | otherwise = 2*n

luhn :: [Int] -> Bool
luhn xs = (sum (altMap id luhnDouble (reverse xs))) `mod` 10 == 0
