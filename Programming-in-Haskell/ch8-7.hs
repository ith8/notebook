data Maybe' a = Nothing' | Just' a
instance Eq a => Eq (Maybe' a) where
  Just' a == Just' b = a == b
  Nothing' == Nothing' = True
  _ == _ = False

instance Eq a => Eq [a] where
  xs == ys = length xs == length ys && and [x == y | (x, y) <- zip xs ys]
  _ == _ = False
