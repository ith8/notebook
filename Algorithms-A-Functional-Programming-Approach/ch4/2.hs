-- with transient list
comp f g xs = [ f x | x <- (map g xs), x > 10]

-- deforestation version
comp' f g (x:xs) 
  | g x > 10 = (f (g x)):(comp' f g xs)
  | otherwise = comp' f g xs

copm'' f g xs = [f (g x) | x <- xs, g x > 10]

