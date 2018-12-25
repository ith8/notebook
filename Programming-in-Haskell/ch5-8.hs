find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k'==k]

--Original positions
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x ==x']

-- Modified positions' in terms of find
positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x (zip xs [0..])
