-- a
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

-- b
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- c
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = a : replicate (n-1) a

-- d. Implement (!!), select n element from a list, recursively
sel :: [a] -> Int -> a
(x:_) `sel` 0 = x
(x:xs) `sel` n =  xs `sel` (n-1)

-- e.
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x' (x:xs) = (x' == x) || elem' x' xs 
