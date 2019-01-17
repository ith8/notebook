choices :: [a] -> [[a]]
choices xs = [ns' | ns <- subs xs, ns' <- perms ns]

subs :: [a] -> [[a]] -- return all possible sub-sequences
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs

interleave :: a -> [a] -> [[a]] -- returns all possible ways of inserting an element into a list
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]] -- all possible permutations of a list
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))
