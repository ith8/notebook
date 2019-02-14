# Concrete Data Types
## Lists
__Pipeline:__ The composition of n list-processing functions ([a] -> [a]).

__Intermediate (transient) list:__ list created in between pipeline functions; Immediately garbage collectable.

__The copy problem:__ 
- Same data replicated instead of sharing the same pointer. => Duplicates memory
- Can be fixed by labeling the data with (@) operator, then reference using the label.

__Tail-strict function:__ 
- Doesn't produce partial results (intermediate lists) but traverses the entire list before producing result.
- May increase space cost from O(1) to O(n) if used in pipelines; where n is the list size.
## Deforestation
Saves cost of creating intermediate list with Burstall-Darlington transformation.

Wadler's deforestation algorithm:
```haskell
double [] = []
double (x:xs) = (2*x):(double xs)

triple [] = []
triple (x:xs) = (3*x):(triple xs) 

--creates transient list:
ldt xs = (double . triple) xs

--after deforestation
ldt' [] = []
ldt' (x:xs) = (6*x):(ldt xs)
```
## Removing Appends
Avoid calls to the (++) operator.

Can be achieved with Burstall-Darlington transformation and a list accumulator.

Increase space and time efficiency from O(n^2) to O(n) in the case of reverse.
```haskell
--with ++ call
reverse [] = []
reverse (x:xs) = (reverse xs) ++ (x:[])

--using list accumulator
reverse' xs = reverse'' xs []
reverse'' [] y = y
reverse'' (x:xs) y = reverse'' xs (x:y)
```
## Reducing Number of Passes
Allow function to traverse list only once.
```haskell
-- traverce twice
average xs = sum xs / fromInt (length xs)

--traverse once with accumulator
average' xs = average'' xs 0 0
average'' [] s n = s / fromInt n
average'' (x:xs) s n = average xs (s+x) (n+1)
```
## Trees
Trees consist of nodes: A root is a node without predecessor; Leaves are nodes without descendants.

For binary trees: 
1. The maximum depth is d = n (a chain of nodes)
2. The minimum depth is d = ceiling (log2 (n+1)).
3. if n = 2^k -1. A perfectly balanced tree has exactly 2^(k-1) leaves and 2^(k-1) - 1 interior nodes.

Just as with lists, composing tree operators can be optimize with deforestation; Burstall-Darlington transformation can be used to avoid multiple traversals and (++) operators; duplicating memory can be avoid by labeling with (@) operator.

In cases where the result of a tree traversal is needed for the second traversal, Bird [11] shows a technique to achieve the computation with a single traversal. 
