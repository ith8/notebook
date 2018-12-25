-- 7.
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
  | x <= y = x :  merge xs (y:ys)
  | otherwise = y :  merge (x:xs) ys

-- 8.
halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
  where n = (length xs) `div` 2
	
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort (fst (halve xs))) (msort (snd (halve xs)))

