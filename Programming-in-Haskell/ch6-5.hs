-- length
length [1,2,3]
  =	{applying length}
1 + length [2,3]
  =	{applying length}
1 + 1 + length [3]
  =	{applying length}
1 + 1 + 1 + length []
  =	{applying length}
1 + 1 + 1 + 0
  =	{applying +}
3

-- drop
drop 3 [1,2,3,4,5]
  =	{applying drop}
drop 2 [2,3,4,5]
  =	{applying drop}
drop 1 [3,4,5]
  =	{applying drop}
drop 0 [3,4,5]
  =	{applying drop}
[3,4,5]

-- init
init [1,2,3]
  =	{applying init}
1 : init [2,3]
  =	{applying init}
1 : 2 : init [3]
  =	{applying init}
1 : 2 : []
  =	{applying :}
[1,2]

