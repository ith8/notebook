-- 1.
answere = sum [x^2 | x <- [1..100]]

-- 2.
grid :: Int -> Int -> [(Int, Int)]
grid x y = [(a,b) | a <- [0..x], b <- [0..y]]

-- 3.
square :: Int -> [(Int, Int)]
square n = [ (a,b) | (a,b) <- grid n n, a /= b]

-- 4.
replicate' :: Int -> a -> [a]
replicate' n a = [ a | _ <- [1..n]]

