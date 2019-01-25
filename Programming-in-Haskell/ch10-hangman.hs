import System.IO

hangman :: IO ()
hangman = do putStrLn "Think of a word:"
	     word <- sgetLine
	     putStrLn "Try to guess it:"
	     play word

sgetLine :: IO String -- getLine but echo '-' in place of characters
sgetLine = do x <- getCh
	      if x == '\n' then
		    do putChar x
		       return []
	      else 
		    do putChar '-'
		       xs <- sgetLine
		       return (x:xs)

getCh :: IO Char
getCh = do hSetEcho stdin False
	   x <- getChar
	   hSetEcho stdin True
	   return x

play :: String -> IO () --main game loop for 2nd player
play word = do putStr "?"
	       guess <- getLine
	       if guess == word then
		  putStrLn "You got it!!"
	       else 
		  do putStrLn (match word guess)
		     play word

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-'| x <- xs]


