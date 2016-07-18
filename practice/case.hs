--lower case char to upper case and upper case to lower case
upperLower :: Char -> Char
upperLower c 
  | elem c ['a'..'z'] = toUpper c
  | elem c ['A'..'Z'] = toLower c
  | otherwise         = c 
  
--fast power
(^) :: Num a => a -> Integer -> a
(^) x n
  | n == 0    = 1
  | odd n     = x * ((^) x (n-1))
  | otherwise = sqr ((^) x (quot n 2))
  
--reverse binary of a positive number
toBin :: Integer -> [Int]
toBin n 
  | n == 0    = []
  | even n    = [0] ++ toBin (quot n 2)
  | otherwise = [1] ++ toBin (quot n 2)
  
--drop
drop :: Int -> [a] -> [a]
drop n (a:as)
  | n <= 0            = (a:as)
  | n > length (a:as) = []
  | otherwise         = drop (n-1) as
  
--take
take :: Int -> [a] -> [a]
take n (a:as)
  | n <= 0             = []
  | n >= length (a:as) = (a:as)
  |otherwise           = [a] ++ take (n-1) as

--insert
insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (a:as)
  | n >= a = [a] ++ insert n as
  | n < a  = n : (a:as)

--sorted lists merge with sorting
sortMerge :: Ord a => [a] -> [a] -> [a]
sortMerge as [] = as
sortMerge [] bs = bs
sortMerge (a:as) (b:bs)
  | a <= b    = [a] ++ sortMerge as (b:bs)
  | otherwise = [b] ++ sortMerge (a:as) bs
