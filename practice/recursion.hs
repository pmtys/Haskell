--sum
sum :: Num a => [a] -> a
sum [] = 0
sum (a:as) =
  a + sum as
  
--last
last :: [a] -> a
last [a] = a
last (a:as) =
  last as
---
last = head . reverse

--every elem without last
init :: [a] -> [a]
init [a] = []
init (a:as) =
  [a] ++ init as
---
init = reverse . tail . reverse

--minimum
minimum :: Ord a => [a] -> a
minimum [a, b] = min a b
minimum (a:as) =
  minimum as
  
--concat
concat :: [[a]] -> [a]
concat [] = []
concat (a:as) =
  a ++ concat as
  
--merge
merge :: [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge (a:as) (b:bs) =
  [a, b] ++ merge as bs
  
--zip
zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (a:as) (b:bs) =
  [(a, b)] ++ zip as bs

--prefix
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (a:as) (b:bs) =
  a == b && isPrefixOf as bs
  
--quick sort
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (a:as) =
  qsort [x | x <- as, x <= a] ++ [a] ++ qsort [x | x <- as, a < x]
  
--tails of a list
tails :: [a] -> [[a]]
tails [] = [[]]
tails as =
  [as] ++ tails (drop 1 as)
  
--all prefixes of a list
inits :: [a] -> [[a]]

---
inits as =
  [take l as | l <- [0..length as]]