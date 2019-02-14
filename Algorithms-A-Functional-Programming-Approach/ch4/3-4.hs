-- 3.
-- a) One-pass version
split ::  Int -> [Int] -> ([Int],[Int])
split x xs = split' x xs [] []

split' :: Int -> [Int] -> [Int] -> [Int] -> ([Int],[Int])
split' _ [] l r = (l,r)
split' x' (x:xs) l r 
  | x <= x' = split' x' xs (x:l) r
  | otherwise = split' x' xs l (x:r)

-- b) The above version is already tail-recursive

-- 4.
-- average'' is derived from average since s and n in av'' are the tail-recursive definition of sum and length (chapter 3). 
