module PQueue (PQueue, enPQ, dePQ, frontPQ, emptyPQ, pqEmpty) where

enPQ :: (Ord a) => a -> (a -> a -> Bool) -> PQueue a -> PQueue a
dePQ :: (Ord a) => PQueue a -> PQueue a
frontPQ :: (Ord a) => PQueue a -> a
emptyPQ :: PQueue a
pqEmpty :: PQueue a -> Bool

newtype PQueue a = PQ[a]

enPQ x comp (PQ q) = PQ (insert x q)
    where insert x [] = [x]
	  insert x r@(e:r') | comp x e = x:r 
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

len (x,y) = sqrt(x^2+y^2)
compfunc p1 p2 = len p1 <= len p2
