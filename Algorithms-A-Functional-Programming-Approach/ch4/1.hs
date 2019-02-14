concat1 xs = foldr (++) [] xs
concat2 xs = foldl (++) [] xs

-- concat1 is more efficient since
-- concat1 takes O(nm) while 
-- concat2 takes O(n^2 m) - the first list would be cons together n times
