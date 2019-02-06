import Data.Array

-- 14.
cube :: Num a => a -> a
maxi :: Ord a => a -> a -> a
sumAtoB :: Integer -> Integer -> Integer

-- 15.
(!) :: Ix i => Array i e -> i -> e
bounds :: Ix i -> Array i e -> (i,i)
indices :: Ix  i -> Array i e -> [i]
elems :: Ix i => Array i e -> [e]

