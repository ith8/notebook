import Data.Array

a1 = listArray (1,4) [11,20,36,47]
a2 = listArray (1,14) [x | x <- [1..15], x /= 9]
a3 = listArray (2,11) [if even x then x else (-x) | x <- [2..11]]
