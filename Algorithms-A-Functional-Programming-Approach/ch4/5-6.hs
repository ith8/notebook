data BinTree a = Empty | NodeBT a (BinTree a) (BinTree a)
    deriving Show

bt = (NodeBT 5 (NodeBT 8 (NodeBT 3 Empty Empty) (NodeBT 1 Empty Empty))(NodeBT 6 Empty (NodeBT 4 Empty Empty)))
bt'= (NodeBT 4 (NodeBT 6 (NodeBT 2 Empty Empty) (NodeBT 1 Empty Empty))(NodeBT 6 Empty (NodeBT 5 Empty Empty)))
bt''=(NodeBT 5 (NodeBT 8 Empty (NodeBT 1 Empty Empty))(NodeBT 6 Empty (NodeBT 4 Empty Empty)))

-- 5.
compareT :: BinTree a -> BinTree a -> Bool
compareT Empty Empty = True
compareT Empty (NodeBT _ _ _) = False
compareT (NodeBT _ _ _) Empty = False
compareT (NodeBT _ l r) (NodeBT _ l' r') = (compareT l l') && (compareT r r')

-- The time efficiency of this function is O(n) where n is the number of nodes.

-- 6.
size :: BinTree a -> Int
size tr = size' tr 0

size' :: BinTree a -> Int -> Int 
size' Empty x = x
size' (NodeBT _ l r) x = 1 + (size' l (size' r x)) 
