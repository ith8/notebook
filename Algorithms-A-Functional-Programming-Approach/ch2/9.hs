import Data.Char

str2int :: String -> Int 
str2int "" = 0 
str2int (x:xs) 
  | isDigit x = (digitToInt x)*10^(length xs) + str2int xs
  | otherwise = error "non digit character"
