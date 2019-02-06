-- 7.
-- a) 
-- [(1,2),(1,4),(1,5),(2,3),(2,4),(2,5)]
-- [2,4,6,8,10]
--
-- b)
l1 :: [Int]
l1 = [x | x <- [1..15], x /= 9]

l2 :: [Int] 
l2 = [if odd x then (-x) else x | x <- [2..11]] 

-- 8.
-- a)
neg :: (Num a, Ord a) => [a] -> Int 
neg = length . filter (\x -> x < 0)

-- b)
rep :: Int -> [Int]
rep n = concat [replicate x x | x <- [1..n]]
