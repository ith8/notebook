data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show)
t :: Tree Int 
t = Node (Node (Leaf 1) (Leaf 2)) (Node (Node (Node (Leaf 8) (Leaf 9)) (Leaf 4)) (Leaf 5))

-- 3.
leaves :: Tree a -> Int
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r) = (diff <= 1) && balanced l && balanced r
  where diff = abs (leaves l - leaves r)

-- 4.
half :: [a] -> ([a],[a])
half xs = (take n xs, drop n xs)
  where n = (length xs) `div` 2

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance (fst (half xs))) (balance (snd (half xs)))
