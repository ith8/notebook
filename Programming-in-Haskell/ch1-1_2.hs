1. double (double 2)
 - double 2 + double 2
 - (2 + 2) + (2 + 2)
 - 4 + 4
 - 8
2.sum [x]
 - x + sum []
 - x + 0
 - x
3. 
product [] = 1
product (n:ns) = n * product ns
4. 
rqsort [] = []
rqsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
		where
		smaller = [a | a <- xs, a <= x]
		larger = [b | b <- xs, b>x]
5. All duplicated entry would be removed. qsort would return and ordered list where every values are present only once.   
