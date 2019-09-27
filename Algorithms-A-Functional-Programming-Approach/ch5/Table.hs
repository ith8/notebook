module Table(Table,newTable,findTable,updTable) where
  import Data.Array 
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
