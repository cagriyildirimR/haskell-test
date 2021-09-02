{-
 - Haskell functions has higher priority than other operators
 - f a + b is same as (f a) + b
 -}

-- Question 2
-- (2^3) * 4
-- (2*3)+(4*5)
-- 2 + (3 * (5^5))

-- Question 3
n = a `div` length xs
    where
      a = 10
      xs = [1..5]

-- Question 4
last' :: [a] -> a
last' = head . reverse 

last'' :: [a] -> a
last'' xs = xs !! (length xs - 1) 

-- Question 5
dInit1, dInit2 :: [a] -> [a]
dInit1 xs = take (length xs - 1) xs
dInit2 xs = reverse (drop 1 (reverse xs))

