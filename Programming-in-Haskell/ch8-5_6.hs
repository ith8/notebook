-- 5
data Expr = Val Int | Add Expr Expr
    deriving (Show)

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add m n) = g (folde f g m) (folde f g n)

-- 6
eval :: Expr -> Int 
eval x = folde id (+) x
