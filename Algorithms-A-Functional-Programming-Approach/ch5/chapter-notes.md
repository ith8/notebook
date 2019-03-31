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

push x (Stack xs) = Stk (x:xs)

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
dequeue :: a -> Queue a -> Queue a
front :: Queue a -> a
emptyQueue :: Queue a
queEmpty :: Queue a -> Bool
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
emptyPQ :: POueue a
pqEmpty :: PQueue a -> Bool
```

List implementation:
- `enPQ` takes up to O(n) in the worst case
- `dePQ` takes O(1)

```haskell
newtype PQueue a = PQ[a] deriving show

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

