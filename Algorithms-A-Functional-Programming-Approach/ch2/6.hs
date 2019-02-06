-- a)
average :: [Float] -> Float
average [] = 0
average xs = (sum xs) / fromIntegral (length xs)

-- b) 
middle :: [a] -> a
middle xs = xs !! n
  where n = length xs `div` 2
