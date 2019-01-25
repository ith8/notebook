adder :: IO ()
adder = do putStr "How many numbers? "
	   n' <- getLine
	   let n = read n' :: Int
	   nums' <- sequence [getLine | _ <- [1..n]]
	   let nums = [read num :: Int | num <- nums']
	   putStr "The total is: "
	   putStr (show (sum nums))
	   putStr "\n"

