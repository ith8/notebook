-- Time analysis: step-counting function Tp
Tp k = if k==0
	  then 1
	  else if k `mod` 2 == 0
	  then 1 + Tp (k `div` 2)
	  else 1 + Tp (k `div` 2)

-- Tp (log2 k=0) = 1
-- Tp (log2 k>0) = 1 + Tp (floor(log2 k -1))
-- => tp(k) = 2 + floor(log2 k)
--
-- Space analysis: cost = 2*floor(log2 k) + 2
