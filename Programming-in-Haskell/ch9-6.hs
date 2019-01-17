data Op = Add | Sub | Mul | Div | Exp
instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"

-- valid: check if output is positive natural
-- modified to remove commutatively identical solutions
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y > 1 && x `mod` y == 0
valid Exp x y = x /= 1 && y /= 1 && y >= 0


apply :: Op -> Int -> Int -> Int 
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

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

-- Brute force solution
split :: [a] -> [([a],[a])] -- all possible ways to split a list
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls, rs) <- split xs]

ops :: [Op]
ops = [Add,Sub,Mul,Div,Exp]

type Result = (Expr,Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns, lx <- results ls, ry <- results rs, res <- combine lx ry]

combine :: Result -> Result -> [Result] -- remove invalid expressions as soon as they are created, before they could be used to create more invalid exprs
combine (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]

-- for performance testing
main :: IO ()
main = print (solutions' [1,3,7,10,25,50] 831)

-- b. Closest approximate solution
solutions' :: [Int] -> Int -> [Expr]
solutions' ns n 
  | length (solutions ns n) /= 0 = solutions ns n
  | otherwise = approx ns n

approx :: [Int] -> Int -> [Expr]
approx ns n = closest result 
  where result = [(e, abs (m-n)) | ns' <- choices ns, (e, m) <- results ns']

closest :: [Result] -> [Expr]
closest [e] = [fst e]
closest (x:y:ys) 
  | snd x < snd y = closest (x:ys)
  | otherwise = closest (y:ys)

-- c. Solution in progress
