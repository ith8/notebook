-- 7.
fft t = (flipT . flipT) t

fft (Leaf a) = flipT (flipT (Leaf a)) 
	     = Leaf a
fft (NodeBT a b) = flipT (flipT (NodeBT a b))
		 = flipT (NodeBT (flipT b) (flipT a))
		 = NodeBT (flipT (flipT a)) (flipT (flipT b))
		 = NodeBt (fft a) (fft b)

-- Since fft left both the structure and the leaf of the tree untouched fft = id.

-- 8.
inorder Empty = []
inorder (NodeBT a l r) = inorder l ++ [a] ++ inorder r

-- #1. Generalize a function inorder'
inorder' t ys = inorder t ++ ys

-- #2. Derive the base case
inorder' Empty ys = inorder Empty ++ ys (line 18)
		  = [] ++ ys (line 14)
                  = ys (a.1)

-- #3. Derive the recursive case 
inorder' (NodeBT a l r) ys = (inorder (NodeBT a l r)) ++ ys (line 18)
			   = inorder l ++ [a] ++ inorder r ++ ys (line 15)
			   = inorder l ++ [a] ++ (inorder' r ys) (line 18)
			   = inorder l ++ a:(inorder' r ys) (property of ++)
			   = inorder' l (a:(inorder' r ys)) (line 18)

-- #4. Complete definition
inorder' Empty ys = ys
inorder' (Node BT a l r) ys = inorder' l (a:(inorder' r ys))

inorder'' t = inorder' t []
