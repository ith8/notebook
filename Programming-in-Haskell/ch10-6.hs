readLine :: IO String
readLine = do x <- getChar
	      if x == '\n' then
		 return []
	      else
		 do y <- getChar
		    if y == '\DEL' then
		       do putChar '\b'
		          putChar '\b'
		          xs <- readLine
		          return xs
		     else
			if y == '\n' then
			   return [x]
			else
			   do xs <- readLine
		              return (x:y:xs)
	         

main :: IO ()
main = do x <- readLine
	  putStrLn (show x)
