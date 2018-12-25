rqsort [] = []
rqsort (x:xs) = rqsort larger ++ [x] ++ rqsort smaller
		where
		smaller = [a | a <- xs, a <= x]
		larger = [b | b <- xs, b>x]