type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
		    putStr ": "
		    putStrLn (concat (replicate num "* "))
-- 2.
putBoard' :: Board -> Int -> IO ()
putBoard' [] _ = return ()
putBoard' (x:xs) row = do putRow row x 
			  putBoard' xs (row+1)

putBoard :: Board -> IO ()
putBoard b = putBoard' b 1

-- 3.
putBoard'' :: Board -> IO ()
putBoard'' xs = sequence_ [putRow r x | (r,x) <- zip [1..] xs]
