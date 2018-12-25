import Data.Char

-- Encoder
let2int :: Char -> Int
let2int c 
  | isLower c = ord c - ord 'a'
  | isUpper c = ord c - ord 'A' + 260

int2let :: Int -> Char
int2let n   
  | n < 260 = chr (n + ord 'a')
  | otherwise = chr( ord 'A' +  n - 260)

shift :: Int -> Char -> Char
shift n c 
  | isLower c = int2let((let2int c + n) `mod` 26)
  | isUpper c = int2let(((let2int c + n) `mod` 26 ) + 260)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
-- to decode, simply encode the encrypted string with, -n, the negative of the shift factor

-- Frequency tables
-- frequency of letters in English
table :: [Float]
table = [8.0, 1.5,2.8,4.2,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,6.7,7.5,1.9,0.1,6.0,6.3,9.0,2.8,1.0,2.4,0.2,2.0,0.1]

-- frequency table calculator
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x' == x]

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
  where n = lowers xs

-- Cipher Cracker
-- chi-square calculator, o = observe, e = expected
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [ ((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- find positions an element in a list
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x' == x]

crack :: String -> String
crack xs = encode (-factor) xs 
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs (map toLower xs)

