module PQueue (PQueue, enPQ, dePQ, frontPQ, emptyPQ, pqEmpty) where

enPQ :: (Ord a) => a -> PQueue a -> PQueue a
dePQ :: (Ord a) => PQueue a -> PQueue a
frontPQ :: (Ord a) => PQueue a -> a
emptyPQ :: PQueue a
pqEmpty :: PQueue a -> Bool

newtype PQueue a = PQ[a]

enPQ x (PQ q) = PQ (insert x q)
    where insert x [] = [x]
	  insert x r@(e:r') | x <= e = x:r 
			    | otherwise = e:insert x r'

dePQ (PQ []) = error "empty priority queue"
dePQ (PQ (x:xs)) = PQ xs

frontPQ (PQ []) = error "empty priority queue"
frontPQ (PQ (x:xs)) = x

emptyPQ = PQ []

pqEmpty (PQ []) = True
pqEmpty _ = False

---------------------------------------
data Point = Pt (Float, Float)
instance Eq Point
  where 
    Pt p1 == Pt p2 = (len p1 == len p2)
instance Ord Point
  where 
    Pt p1 <= Pt p2 = (len p1 <= len p2)

len (x,y) = sqrt(x^2+y^2)
