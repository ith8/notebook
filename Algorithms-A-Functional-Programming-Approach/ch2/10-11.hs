-- 10. 
-- [1,3,0,3]
-- (6,7) :
foo = (foldr f 0 l, foldl f 0 l)
  where l = [6,9,8,3,10]
	f x y = (x+y) `div` 2
-- [1,2,3,4,5,6,7]

-- 11.
-- compose :: (b -> c) -> (a -> b) -> (a -> c)


