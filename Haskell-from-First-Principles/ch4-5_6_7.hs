-- 5
myand :: Bool -> Bool -> Bool
b `myand` c = if b then (if c then True else False) else False

-- 6
myand' :: Bool -> Bool -> Bool
c `myand'` b = if c then b else False

-- 7
mymult :: Int -> (Int -> (Int -> Int))
mymult = \x -> ( \y -> ( \z -> x*y*z))

