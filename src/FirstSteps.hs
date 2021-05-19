-- (2^3) * 4
-- (2*3)+(4*5)
-- 2 + (3 * (5^5))

n = a `div` length xs
    where
      a = 10
      xs = [1..5]

dLast :: [a] -> [a]
dLast xs = take 1 (reverse xs)

dInit1, dInit2 :: [a] -> [a]
dInit1 xs = take (length xs - 1) xs
dInit2 xs = reverse (drop 1 (reverse xs))