-- 1.
--[f x | x <- xs, p x] = map f (filter p xs)

-- 2.
-- a
all' :: (a -> Bool) -> [a] -> Bool
all' p xs = and (map p xs)

-- b
any' :: (a -> Bool) -> [a] -> Bool
any' p xs = or (map p xs)

-- c
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x = x: takeWhile' p xs
  | otherwise  = []

-- d 
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) 
  | p x = dropWhile' p xs
  | otherwise = x:xs
