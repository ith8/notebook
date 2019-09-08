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

inSet _ (St []) = False
inSet y (St (x:xs)) | y == x = True
		    | otherwise = inSet y (St xs)

addSet x (St a) = St (x:a)

delSet y (St x) = St (delS y x)
  where delS _ [] = []
        delS y (x:xs)  | y == x = delS y xs
                       | otherwise = x : delS y xs
    
instance (Show a) => Show (Set a) where
        showsPrec _ (St s) str = showSet s str

showSet [] str = showString "{}" str
showSet (x:xs) str = showChar '{' ( shows x ( showl xs str))
  where showl [] str = showChar '}' str
        showl (x:xs) str = showChar ',' (shows x (showl xs str)) 
