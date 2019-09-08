module Set (Set, emptySet, setEmpty, inSet, addSet, delSet) where

emptySet :: Set a 
setEmpty :: Set a -> Bool
inSet     :: (Eq a) => a -> Set a -> Bool  
addSet    :: (Eq a) => a -> Set a -> Set a 
delSet    :: (Eq a) => a -> Set a -> Set a

newtype Set a = St [a]

emptySet = St []

setEmpty (St []) = True
setEmpty _       = False

inSet x (St xs) = length [a | a <- xs, a == x] /= 0

addSet x (St a) = St (x:a)

delSet x (St xs) = St (filter (/= x) xs)

included (St s1) (St s2) = and [elem x s2 | x <- s1] -- true if all elem in s1 are in s2

union (St s1) (St s2) = s1 ++ [x | x <- s2, not (elem x s2)]

inter (St s1) (St s2) = [x | x <- s1, elem x s2] -- intersection of s1 and s2

    
instance (Show a) => Show (Set a) where
      showsPrec _ (St s) str = showSet s str

showSet [] str = showString "{}" str
showSet (x:xs) str = showChar '{' ( shows x ( showl xs str))
  where showl [] str = showChar '}' str
	showl (x:xs) str = showChar ',' (shows x (showl xs str)) 
