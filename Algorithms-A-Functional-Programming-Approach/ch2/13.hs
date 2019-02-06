import Data.Array

-- a)
m :: Array (Int, Int) Int 
m = array ((1,1),(3,3)) [((1,1), 2), ((1,2),5), ((1,3),8), ((2,1),3), ((2,2),6), ((2,3),9), ((3,1),4), ((3,2),7), ((3,3),10)]

-- b)
m' :: Array (Int, Int) Int 
m' = array ((1,1),(3,3)) [((i,j), x) | ((i,j),x) <- zip [(i,j) | j <- [1..3], i <- [1..3]] [2..10]] 

-- c)
transpose3 :: Array (Int, Int) Int -> Array (Int,Int) Int
transpose3 a = array ((1,1),(3,3)) [((i, j), a!(j,i)) | i <- [1..3], j <- [1..3]]

-- d)
transpose :: Array (Int, Int) Int -> Array (Int,Int) Int
transpose a = array ((j0,i0),(j',i')) [((i, j), a!(j,i)) | i <- [1..i'], j <- [1..j']]
  where ((i0,j0),(i',j')) = bounds a
