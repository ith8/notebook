unfold p h t x 
  | p x = []
  | otherwise = h x : unfold p h t (t x)

int2bin :: Int -> [Int]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

-- chop8
chop8 :: [Int] -> [[Int]]
chop8 = unfold (null) (take 8) ( drop 8)

-- map f
map' :: (a -> b) -> [a] -> [b]
map' f = unfold (null) (f . head) tail

-- iterate f
iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\_ -> True) id f
