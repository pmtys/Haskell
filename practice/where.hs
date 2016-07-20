--unzip
unzip :: [(a, b)] -> ([a], [b])
unzip [] = ([], [])
unzip ((f, s):as) =
  (f : fst aa, s : snd aa)
  
  where 
    aa = unzip as
	
--splitAt
splitAt :: Int -> [a] -> ([a], [a])
splitAt n (a:as)
  | n <= 0             = ([], (a:as))
  | n >= length (a:as) = ((a:as), [])
  | otherwise          = ([a] ++ fst aa, [] ++ snd aa)

  where
    aa = splitAt (n-1) as
	
--split
split :: [a] -> ([a], [a])
split []  = ([], [])
split [a] = ([a], [])
split (a:aa:as) =
  (a : fst aaa, aa : snd aaa)
    where
      aaa = split as