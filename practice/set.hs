--numbers 1 to 2^10
[2^n | n <- [0..10]]

--first power of 2 what is greater than 10^20
head [2^n | n <- [1..], 2^n > 10^20]

--numbers of 60 divisors
length [d | d <- [1..60], mod 60 d == 0]

--dominos
[(x, y) | x <- [0..9], y <- [x..9]]

--NN x NN
[(x, y) | s <- [0..], x <- [0..s], y <- [0..s], x + y == s]

--[1,2,2,3,3,3,4,4,4,4, ..]
concat [take i [i, i..] | i <- [1..]]

--"* ** *** **** ***** .."
unwords [replicate i '*' | i <- [1..]]

--NN\{square numbers}
[n | n <- [1..], even $ length [d | d <- [1..n], mod n d == 0]] :: [Integer]
---
[n | n <- [1..],  not $ sqrt n * sqrt n == n] :: [Double]