-- 5. 
-- generate candidate Pythagorean triplets (without duplicate candidates)
perm :: Int ->  [(Int, Int, Int)]
perm n = [(z,y,x) | x <- [1..n], y <- [1..(x-1)], z <- [1..(y-1)]]

-- filter Pythagorean triplets
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | (x,y,z) <-  perm n, x^2 + y^2 == z^2]

-- Solutions (has duplicates triplets)
pyths' :: Int -> [(Int, Int, Int)]
pyths' n = [(x, y, z) |
    x <- [1..n],
    y <- [1..n],
    z <- [1..n],
    x^2 + y^2 == z^2]
