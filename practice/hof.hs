--map
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (a:as) =
  f a : map f as

--filter
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (a:as)
  | f a       = a : filter f as
  | otherwise = filter f as

--count
count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count f (a:as)
  | f a       = 1 + count f as
  | otherwise = count f as

--takeWhile
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (a:as)
  | f a       = a : takeWhile f as
  | otherwise = []
  
--dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f (a:as)
  | f a       = dropWhile f as
  | otherwise = (a:as)
  
--span f as (takeWhile f as, dropWhile f as)
span :: (a -> Bool) -> [a] -> ([a], [a])
span _ [] = ([], [])
span f (a:as)
  | f a       = ([a] ++ fst aa, [] ++ snd aa) 
  | otherwise = ([], (a:as))
  where
    aa = span f as
	
--all
all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all f (a:as) =
  f a && all f as
  
--any
any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any f (a:as) =
  f a || any f as
  
--elem with any
elem :: Eq a => a -> [a] -> Bool
elem e as =
  any (==e) as
  
--filters
filters :: Eq a => [a] -> [a] -> [a]
filters l1 l2 =
  [c | c <- l2, notElem c l1]
  
--zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (a:as) (b:bs) =
  f a b : zipWith f as bs
  
