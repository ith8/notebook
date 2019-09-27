module Stack (Stack, push, pop, top, emptyStack, stackEmpty) where 
   push :: a -> Stack a -> Stack a
   pop :: Stack a -> Stack a
   top :: Stack a -> a 
   emptyStack :: Stack a 
   stackEmpty :: Stack a -> Bool
   
   newtype Stack a = Stk [a]
   
   push x (Stk xs) = Stk (x:xs)
   
   pop (Stk []) = error "empty stack"
   pop (Stk (_:xs)) = Stk xs
   
   top (Stk []) = error "empty stack"
   top (Stk (x:_)) = x
   
   emptyStack = Stk []
   
   stackEmpty (Stk []) = True
   stackEmpty (Stk _) = False
   
   instance (Show a) => Show (Stack a) where
         showsPrec p (Stk []) str = showChar '-' str
         showsPrec p (Stk (x:xs)) str = shows x (showChar '|' (shows (Stk xs) str))
