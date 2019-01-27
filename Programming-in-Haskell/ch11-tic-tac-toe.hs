import Data.Char
import Data.List
import System.IO

-- Basic game mechanics
size :: Int --Size of grid
size = 3

type Grid = [[Player]]

data Player = O | B | X
    deriving (Eq, Ord, Show)

next :: Player -> Player --Determining turns
next O = X
next B = B -- Never actually use
next X = O

-- Grid Utilities
empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X -- O always go first
	 where 
	   os = length (filter (== O) ps)
	   xs = length (filter (== X) ps)
	   ps = concat g

wins :: Player -> Grid -> Bool 
wins p g = any line (rows ++ cols ++ dias) --player wins if any list contain a triplet
	   where 
	     line = all (==p)
	     rows = g
	     cols = transpose g
	     dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool -- check if the game is won
won g = wins O g || wins X g

-- Displaying Grid
putGrid :: Grid -> IO () -- impure
putGrid =
    putStrLn . unlines . concat . interleave bar . map showRow
      where bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
	  where 
	    beside = foldr1 (zipWith (++))
	    bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

-- Making moves
valid :: Grid -> Int -> Bool -- checking if a move is valid
valid g i = i >= 0 && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid] --return empty list if move is invalid
move g i p =
    if valid g i then [chop size (xs ++ [p] ++ ys)] else []
      where (xs, B:ys) = splitAt i (concat g) -- replace B with player

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs) -- undo concat g

-- Reading move
getNat :: String -> IO Int
getNat prompt = do putStr prompt
		   xs <- getLine
		   if xs /= [] && all isDigit xs then
		      return (read xs)
		   else
		      do putStrLn "ERROR: Invalid number"
			 getNat prompt

-- Main loop for Human-Human game
tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do cls
	     goto (1,1)
	     putGrid g
	     run' g p

run' :: Grid -> Player -> IO ()
run' g p 
  | wins O g = putStrLn "Player O wins! \n"
  | wins X g = putStrLn "Player X wins! \n"
  | full g = putStrLn "It's a draw! \n"
  | otherwise =
      do i <- getNat (prompt p)
	 case move g i p of 
	     [] -> do putStrLn "ERROR: Invalid move"
		      run' g p
	     [g'] -> run g' (next p)

prompt :: Player -> String 
prompt p = "Player " ++ show p ++ ", enter your move: "

-- Ch10: life game utils
cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Computer v. Human Game Tress
data Tree a = Node a [Tree a]
    deriving Show

gametree :: Grid -> Player -> Tree Grid -- Generate tree of all possible boards
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid] -- list of all possible moves
moves g p 
  | won g = []
  | full g = []
  | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

-- Pruning tree, limit depth
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int 
depth = 9

-- Minimax algorithm
minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
  | wins O g = Node (g,O) []
  | wins X g = Node (g,X) []
  | otherwise = Node (g,B) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
		  where 
		    ts' = map minimax ts
		    ps = [p | Node (_,p) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
	       where 
		 tree = prune depth (gametree g p)
		 Node (_,best) ts = minimax tree

-- Main Game Loop
main :: IO ()
main = do hSetBuffering stdout NoBuffering
	  play empty O

play :: Grid -> Player -> IO ()
play g p = do cls
	      goto (1,1)
	      putGrid g
	      play' g p 

play' :: Grid -> Player -> IO ()
play' g p
   | wins O g = putStrLn "Player O wins! \n"
   | wins X g = putStrLn "Player X wins! \n"
   | full g = putStrLn "It's a draw! \n"
   | p == O = do i <- getNat (prompt p)
		 case move g i p of
		     [] -> do putStrLn "ERROR: Invalid move"
			      play' g p
                     [g'] -> play g' (next p)
   | p == X = do putStr "Player X is thinking... "
		 (play $! (bestmove g p)) (next p) -- $! operator guarantees bestmove evaluates before play


