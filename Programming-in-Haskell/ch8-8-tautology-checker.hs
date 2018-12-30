-- Proposition data type
data Prop = Const Bool 
	  | Var Char 
	  | Not Prop 
          | And Prop Prop 
          | Imply Prop Prop
	  | Or Prop Prop -- prob 8
	  | Equiv Prop Prop
-- Examples propositions
p1 :: Prop 
p1 = And (Var 'A') (Not (Var 'A'))
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 :: Prop
p4 = Imply ( And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

-- Evaluation of propositions
-- Association list
type Assoc k v = [(k,v)]
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k'==k]

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q) = eval s p || eval s q -- prob 8
eval s (Equiv p q) = eval s p == eval s q

-- Generate Association list of all possible truth values 
-- Find all variables in a proposition
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q
-- Generate list of all possible combinations of n truth values 
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
  where bss = bools (n-1)
-- Generate possible association list
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = rmdups (vars p)

-- Tautology checker
isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]
