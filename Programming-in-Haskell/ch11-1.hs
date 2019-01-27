-- Count maximum number of nodes for a given tree
nodes :: Tree a -> Int
nodes (Node x []) = 1
nodes (Node x ts) = 1 + sum (map nodes ts)

> nodes (gametree empty O)
549946

-- Count maximum depth of a given tree
depths :: Tree a -> Int
depths (Node x []) = 0
depths (Node x ts) = 1 + maximum [ depths t | t <- ts]

> depths (gametree empty O)
9

