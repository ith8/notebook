module Set (Set, emptySet, setEmpty, inSet, addSet, delSet) where

emptySet :: Set a 
setEmpty :: Set a -> Bool
inSet  :: (Ord a) => a -> Set a -> Bool  
addSet :: (Ord a) => a -> Set a -> Set a 
delSet :: (Ord a) => a -> Set a -> Set a 

newtype Set a = St [a]

emptySet = St []

setEmpty (St []) = True
setEmpty _       = False

inSet x (St s) = elem x (takeWhile (<= x) s)

addSet x (St s) = St (add x s)
    where add x []                   = [x]                
	  add x s@(y:ys)| (x>y)      = y : (add x ys)
		        | (x<y)      = x : s
			| otherwise  = s

delSet x (St s) = St (del x s)
    where del x []                   = []
	  del x s@(y:ys)| (x>y)      = y : (del x ys)
			| (x<y)      = s
			| otherwise  = ys

inter (St s1) (St s2) = St (intr s1 s2)
  where intr s [] = []
	intr [] s = []
	intr s1@(x:xs) s2@(y:ys) | x==y = x : (intr xs ys)
		                 | x> y = intr s1 ys
		                 | otherwise = intr s2 xs

union (St s1) (St s2) = St (unn s1 s2)
  where unn s [] = s
	unn [] s = s
	unn s1@(x:xs) s2@(y:ys) | x==y = x : (unn xs ys)
		                | x> y = y : (unn s1 ys)
		                | otherwise = x : (unn s2 xs)
    
instance (Show a) => Show (Set a) where
        showsPrec _ (St s) str = showSet s str

showSet [] str = showString "{}" str
showSet (x:xs) str = showChar '{' ( shows x ( showl xs str))
  where showl [] str = showChar '}' str
	showl (x:xs) str = showChar ',' (shows x (showl xs str)) 
