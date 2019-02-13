f a b 
(f $! a) b -- strict evaluation of a only
(f a) $! b -- strict evaluation of b only
(f $! a) $! b -- strict evaluation of both
