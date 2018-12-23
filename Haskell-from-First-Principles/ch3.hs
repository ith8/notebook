 {- 1.
 - ['a','b','c'] :: [Char]
 - ('a','b','c') :: (Char, Char, Char)
 - [(False, '0'), (True, '1')] :: [(Bool, Char)]
 - ([False, True], ['0','1']) :: ([Bool], [Char])
 - [tail, init, reverse] :: [[a] -> [a]] --
  -}
 -- 2.
 bools :: [Bool]
 bools = [True, True, False]
 nums :: [[Int]]
 nums = [ [2,3,4],[2],[3,3,8]]
 add :: Int -> Int -> Int -> Int
 add x y z = x + y +z
 copy :: a -> (a,a)
 copy a = (a,a)
 apply :: (a -> b) -> a -> b
 apply f a = f a
   where f a = (a,a)
-- 3.
second :: [a] -> a
second xs = head (tail xs)
swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)
pair :: a -> b -> (a,b)
pair x y = (x,y)
double :: Num a => a -> a
palindrome :: Eq a => [a] -> Bool --
palindrome xs = reverse xs == xs
twice :: ( a -> a ) -> a -> a
twice f x = f (f x)
{- 5. Function types are not instances of the Eq class since in order to compare two function, and verify whether they are equal, you would have to test all possible inputs (which may be infinite) and see if all their outputs are equal. It may be feasible for Function types to be instances of the Eq class if the set of inputs are limited. For example, only Int between 2 and 5, or only Char before 'g'. 
    -}
