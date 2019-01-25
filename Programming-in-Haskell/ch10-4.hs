adder' :: Int -> Int -> IO Int
adder' tot n = do x' <- getLine
		  let  x = read x' :: Int
		  if n == 1 then
		     return (tot + x)
		  else
		     adder' (tot + x) (n-1)

-- 4.
adder :: IO ()
adder = do putStr "How many numbers? "
	   n' <- getLine
	   let n = read n' :: Int
	   sum <- adder' 0 n
	   putStr "The total is: "
	   putStr (show sum)
	   putStr "\n"
