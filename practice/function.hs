--[1, 2, ..., n-1, n, n-1, ..., 2, 1]
mountain :: Integer -> [Integer]
mountain n =
  [1..n] ++ [n-1, n-2..1]
  
--it can be triangle?
areTriangleSides :: Real a => a -> a -> a -> Bool
areTriangleSides a b c =
  a+b > c && a+c > b && b+c > a
  
--define even
even :: Integer -> Bool
even n =
  mod n 2 == 0
  
--divisibility
divides :: Integer -> Integer -> Bool
divides d n =
  mod n d == 0
  
--divisors
divisors Integer -> [Integer]
divisors n =
  [d | d <- [1..n], divides d n]
  
--sum of numbers (0 to n) square
sumSquaresTo n | n <= 0 = 0
sumSquaresTo n =
  sum [n^2 | n <- [1..n]]