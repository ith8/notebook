factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfect :: Int -> Bool 
perfect n = sum (factors n) - n == n

-- find all perfect numbers up to n
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], perfect x]
