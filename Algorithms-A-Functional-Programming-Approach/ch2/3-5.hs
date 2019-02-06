-- 3.
-- 1:2:3 -> 3 is not a list. 
-- Should be either 1:2:3:[] or 1:2:[3]

-- [[2,3]++[], [2,3]:[]] -> [2,3]++[], is of type [Int], while [2,3]:[], its head is a list so its type is [[Int]].
-- Should be [[2,3]++[], 2:3:[]] or [[[2,3]]++[], [2,3]:[]]

-- "hello":"world" -> the head is of type String so the last element should be of type [String].
-- Should be "hello":"world":[] or "hello":["world"]


-- 4.
f l = reverse (f' l [])
  where f' [] r = r
	f' (x:xs) r = (2*x) : (f' xs r)
-- f [1,2,3,4] = [8,6,4,2]


-- 5.
-- [1,2,3] ++ [4] = [1,2,3,4]
-- 1:(2:(3:[4])) = [1,2,3,4]
-- head [1,2,3] = 1
-- tail [1,2,3] = [2,3]
-- drop 4 [1,2,3,4,5] = [5]
-- [1,2,3,4] !! 2 = 3

