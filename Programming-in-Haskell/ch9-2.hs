isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice xs (y:ys) = isChoice rest ys
  where rest = remove1st y xs

remove1st :: Eq a => a -> [a] -> [a]
remove1st n xs 
  | elem n xs =  takeWhile (/= n) xs ++ tail (dropWhile (/= n) xs)
  | otherwise = xs 
