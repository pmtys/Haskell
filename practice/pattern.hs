--(||)
(||) :: Bool -> Bool -> Bool
(||) True _ = True
(||) _ True = True
(||) _ _    = False

--xor
xor :: Bool -> Bool -> Bool
xor False True = True
xor True False = True
xor _ _        = False
---
xor a b =
  a /= b
--
xor = (/=)

--replace '\n' to ' '
replaceNewline :: Char -> Char
replaceNewline '\n' = ' '
replaceNewline x    = x

--replace '\n' to ' ' in a string
replaceNewlines str =
  [replaceNewline c | c <- str]
  
--is it singleton?
isSingleton :: [a] -> Boool
isSingleton [a] = True
isSingleton _   = False

--count of "a"
countOfAs :: String -> Int
countOfAs str =
  length [a | a <- words str, a == "a"]
  
--every fifth elem of a list
everyFifth :: [a] -> [a]
everyFifth l =
  [x | (x, y) <- zip l [0..], mod y 5 == 0]
  
