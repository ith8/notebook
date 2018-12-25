-- a
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

-- b
safetail' :: [a] -> [a]
safetail' xs 
  | null xs = []
  | otherwise = tail xs

-- c
safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (_ : xs) = xs
