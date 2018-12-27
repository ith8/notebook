map' :: (a -> b) -> [a] -> [b]
map' f = foldr f' []
  where f' x xs = [f x] ++ xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr f' []
  where f' x xs | p x = [x] ++ xs
	  | otherwise = xs
