# Abstract Data Types (ADT)

Interface: type definition and specification of an ADT's operations

Haskell module: 
- collection of functions and type definition in a closed environment.
- can export all or some of its definitions.

```haskell
module name (export list) where
```
## Stacks
Last In First Out (LIFO)

ADT module:

```haskell
module Stack (Stack, push, pop, top, emptyStack, stackEmpty) where

push :: a -> Stack a -> Stack a
pop :: Stack a -> Stack a
top :: Stack a -> a 
emptyStack :: Stack a 
stackEmpty :: Stack a -> Bool
```

Implementing `Stack` with lists using `newtype`:

```haskell
newtype Stack a = Stk [a] deriving Show

push x (Stk xs) = Stk (x:xs)

pop (Stk []) = error "empty stack"
pop (Stk (_:xs)) = Stk xs

top (Stk []) = error "empty stack"
top (Stk (x:_)) = x

emptyStack = Stk []

stackEmpty (Stk []) = True
stackEmpty (Stk _) = False
```

Displaying a `Stack` without revealing internal definition.

Ex: `push 3 (push 2 (push 1 emptyStack))` => 3|2|1|-

```haskell
instance (Show a) => Show (Stack a) where
    showsPrec p (Stk []) str = showChar '-' str
    showsPrec p (Stk (x:xs)) str = shows x (showChar '|' (shows (Stk xs) str))
```
## Queues

First In First Out (FIFO)

ADT module:

```haskell
module Queue (Queue, enqueue, dequeue, front, emptyQueue, queueEmpty) where

enqueue :: a -> Queue a -> Queue a
dequeue :: PQueue a -> Queue a
front :: Queue a -> a
emptyQueue :: Queue a
queueEmpty :: Queue a -> Bool
```

Queue implementation with single list: enqueue would take O(n) to insert item at the end of the list.

Queue implementation with pair of list:
- one representing the front
- the other representing the rear in reverse order
- enqueue takes O(1) steps
- dequeue takes O(1) steps on except when the first list is empty -> worst case is O(n).

```haskell
newtype Queue a = Q ([a],[a]) deriving show

enqueue x (Q ([],[])) = Q ([x], [])
enqueue y (Q (xs, ys)) = Q (xs, y:ys)

dequeue (Q ([],[])) = error "empty queue"
dequeue (Q (x:xs, ys)) = Q (xs,ys)
dequeue (Q ([],ys)) = Q (tail (reverse ys), [])

front (Q ([],[])) = error "empty queue"
front (Q (x:xs,ys)) = x
fonnt (Q (x:xs, ys)) = last ys

queueEmpty (Q ([],[])) = True
queueEmpty _ = False

emptyQueue = Q ([],[])
```

Displaying:

```haskell
instance (Show a) => Show (Queue a) where 
    showsPrec p (Q (front, rear)) str = showString "Q " (showList (front ++ reverse rear) str)
```

## Priority queues

`dequeue` operation always remove the item with the highest priority.

ADT module:

Ordering is based on the (<=) operator. Smallest item has the highest priority.

```haskell
module PQueue (PQueue, enPQ, dePQ, frontPQ, emptyPQ, pqEmpty) where

enPQ :: (Ord a) => a -> PQueue a -> PQueue a
dePQ :: (Ord a) => PQueue a -> PQueue a
frontPQ :: (Ord a) => PQueue a -> a
emptyPQ :: PQueue a
pqEmpty :: PQueue a -> Bool
```

List implementation:
- `enPQ` takes up to O(n) in the worst case
- `dePQ` takes O(1)

```haskell
newtype PQueue a = PQ[a] deriving Show

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
```
## Sets
A collection of distinct items in which:
- an item can be tested for membership
- an item can be inserted or deleted from the set
- the number of distinct item is the size of the set

ADT module:
```haskell
module Set (Set, inSet, addSet, delSet, emptySet, setEmpty) where

inSet :: (Eq a) => a -> Set a -> Bool
addSet :: (Eq a) => a -> Set a -> Set a
delSet :: (Eq a) => a -> Set a -> Set a
emptySet :: Set a
setEmpty :: Set a -> Bool
```

Implementation with unordered list without duplicates:

- `addset` takes O(n) steps
- `delSet` does not need to traverse the whole set if there's no duplicate

```haskell
newtype Set a = St [a] deriving show

inSet x (St xs) = elem x xs

addSet x s@(St xs) | inSet x s = s
		   | otherwise = St (x:xs)

delSet x (St s) = St (delete x s)

emptySet = St []

setEmpty (St []) = True
setEmpty _ = False
```

Displaying:

```haskell
instance (Show a) => Show (Set a) where
    showsPrec _ (St s) str = showSet s str

showSet [] str = showString "{}" str
showSet (x:xs) str = showChar '{' (shows x (showl xs str))
    where showl [] str = showChar '}' str
	  showl (x:xs) str = showChar ',' (shows x (showl xs str))
```

## Tables
Stores and retrieves values according to an index. 
- implements a function of type `(b->a)` with a data structure instead of an algorithm
- `newTable` takes a list of (index, value) pairs and returns a table

```haskell
module Table(Table,newTable,findTable,updTable) where

newTable    :: (Eq b) => [(b,a)] -> Table a b
findTable   :: (Eq b) => Table a b -> b -> a
updTable    :: (Eq b) => (b,a) -> Table a b -> Table a b
```

Function implementation:

- creates a new function whenever a new item x with index i is inserted.
- such f' returns x for index i and f j for all other j.
- `findTable` has smaller runtime for and index the more recent that index is updated. Worst case is linear time.

```haskell
newtype Table a b   = Tbl (b -> a)

instance Show (Table a b) where
    showsPrec _ _ str = showString "<<A Table>>" str

    newTable assocs = foldr updTable (Tbl (\_ -> error "item not found in table")) assocs

    findTable (Tbl f) i   = f i

    updTable (i,x) (Tbl f) = Tbl g
	where g j | j==i      = x
		  | otherwise = f j
```

List implementation:

- use association list
- inefficient since `updTable` and `findTable` is linear in the worst case.
- can improve by sorting the items but runtime remains linear in the worst case.

```haskell
newtype Table a b        = Tbl [(b,a)]
    deriving Show

    newTable   t          = Tbl t

    findTable (Tbl []) i = error "item not found in table"
    findTable (Tbl ((j,v):r)) i
         | (i==j)        = v
	 | otherwise     = findTable (Tbl r) i 

    updTable e (Tbl [])         = (Tbl [e])
    updTable e'@(i,_) (Tbl (e@(j,_):r))
	 | (i==j)         = Tbl (e':r)
	 | otherwise      = Tbl (e:r')
	    where Tbl r' = updTable e' (Tbl r)

```

Array implementation:
- restrict index to class `Ix`. 
- no new values outside the initial boundary.
- `newTable` determines the boundary by computing the min and max key of the association list.
- efficiency depends on array implementation. At best, access takes constant time but array cannot be updated in place.

```haskell
newTable    :: (Ix b) => [(b,a)] -> Table a b
findTable   :: (Ix b) => Table a b -> b -> a
updTable    :: (Ix b) => (b,a) -> Table a b -> Table a b

newtype Table a b     = Tbl (Array b a)
    deriving Show

    newTable l = Tbl (array (lo,hi) l)
        where
	    indices = map fst l
	    lo      = minimum indices
	    hi      = maximum indices

    findTable (Tbl a) i      = a ! i

    updTable p@(i,x) (Tbl a) = Tbl (a // [p])
```
