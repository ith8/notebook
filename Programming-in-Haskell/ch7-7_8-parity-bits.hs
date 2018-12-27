import Data.Char

-- Binary digits (in reverse)
type Bit = Int

-- Conversion Binary to Decimal
bin2int :: [Bit] -> Int
{- bin2int bits = sum [w*b | (w,b) <- zip weights bits]
  where wheights = iterate (*2) 1
	-}
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- 7. All bits have length 8 + 1 parity bit
make8 :: [Bit] -> [Bit]
make8 bits = (take 8 (bits ++ repeat 0))++ [if odd (sum bits) then 1 else 0] 

-- Transmission
encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

-- spit bits and check parity bit
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = (if even (sum (take 9 bits)) then take 8 bits else error "corrupted message") : chop8 (drop 9 bits) 

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . faultyChannel . encode

channel :: [Bit] -> [Bit]
channel = id

-- 8.
faultyChannel :: [Bit] -> [Bit]
faultyChannel = tail

