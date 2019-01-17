data Op = Add | Sub | Mul | Div
instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

-- valid: check if output is positive natural
valid :: Op -> Int -> Int -> Bool
{- valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0 -}
-- modified to remove commutatively identical solutions
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0 

apply :: Op -> Int -> Int -> Int 
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- Numeric expressions
data Expr = Val Int | App Op Expr Expr
instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where 
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r 

eval :: Expr -> [Int] -- invalid ans would returned empty list
eval (Val n) = [ n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

-- Combinatorial Functions
subs :: [a] -> [[a]] -- return all possible sub-sequences
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs 

interleave :: a -> [a] -> [[a]] -- returns all possible ways of inserting an element into a list
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]] -- all possible permutations of a list
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- choices: all possible ways of selecting 0-n elements of a list
choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- Formalizing the problem, solution checker
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
    elem (values e) (choices ns) && eval e == [n]

-- Brute force solution
split :: [a] -> [([a],[a])] -- all possible ways to split a list
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr] -- all possible expressions
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]
combine :: Expr -> Expr -> [Expr] -- all possible ways to apply operators to 2 expressions
combine l r = [App o l r | o <- ops]
ops :: [Op]
ops = [Add,Sub,Mul,Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
    [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

-- for performance testing
main :: IO ()
main = print (solutions' [1,3,7,10,25,50] 765)

-- Efficient solutions
type Result = (Expr,Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns, lx <- results ls, ry <- results rs, res <- combine' lx ry]

combine' :: Result -> Result -> [Result] -- remove invalid expressions as soon as they are created, before they could be used to create more invalid exprs
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]
