import Data.List

{- names are typed wrong because of prelude naming conflicts -}

double :: Int -> Int
double n = n * 2

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

perfect :: Int -> Bool
perfect n = n == (sum [x | x <- [1..n-1], n `mod` x == 0])

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], perfect x]

--scalar :: Num a => [a] -> [a] -> a
scalar xs ys = sum [ x * y | (x,y)<- zip xs ys ]

isVovel :: Char -> Bool
isVovel c = or [c == v | v <- ['a','e','i','o','u']]

disemvowel :: String -> String
disemvowel str = [c | c <- str, not (isVovel c)]

lngth :: [a] -> Int
lngth [] = 0
lngth (_:xs) = 1 + lngth xs

-- assume list has equal or more than two elements
edges :: [Int] -> [Int]
edges xs = [head xs] ++ [head (reverse xs)]

-- assume order is not important
trimEdges :: [Int] -> [Int]
trimEdges xs = tail (reverse (tail xs))

gaussSum :: [Int] -> Int
gaussSum xs | length xs == 0 = 0
            | length xs == 1 = (xs !! 1)
            | otherwise = sum (edges xs) + gaussSum (trimEdges xs)

rvrs :: [a] -> [a]
rvrs [] = []
rvrs (x:xs) = rvrs xs ++ [x]

--fac :: Num a => a -> a
--fac 1 = 1
--fac n = fac (n-1) * n

--fac n | n == 1 = 1
--      | otherwise fac (n-1) * n

aand :: [Bool] -> Bool
aand [] = True
aand (False:_) = False
aand [True] = True
aand (_:bs) = aand bs

-- and (b:bs) = b && and bs

konkat :: [[a]] -> [a]
konkat [] = []
konkat (xs:xss) = xs ++ konkat xss

replikate :: Int -> a -> [a]
replikate 0 _ = []
replikate n x = [x] ++ replikate (n-1) x
-- replikate n x = x : replikate (n-1) x

-- assume index is always i >= 0 and i < length xs
(!!!!) :: [a] -> Int -> a
(!!!!) (x:_) 0 = x
(!!!!) (x:xs) n = xs !!!! (n-1)

eleme :: Eq a => a -> [a] -> Bool
eleme e [] = False
eleme e (x:xs) | e == x = True
               | otherwise = eleme e xs


dooble :: Num a => a -> a
dooble x = x + x

-- assume inputs are two ordered list
merges :: [Int] -> [Int] -> [Int]
merges xs [] = xs
merges [] ys = ys
merges (x:xs) (y:ys) = if x <= y then x : merges xs (y:ys) else y : merges (x:xs) ys
--merge (x:xs) (y:ys) | x > y =  y:(merge (x:xs) ys)
--                    | otherwise =  x:(merge xs (y:ys))

inzert :: Int -> [Int] -> [Int]
inzert n xs = smaller ++ [n] ++ larger
                where
                  smaller = [a | a <- xs, a <= n]
                  larger  =  [b | b <- xs, b > n]

insRec :: Int -> [Int] -> [Int]
insRec n [] = [n]
insRec n (x:xs) | n <= x = [n] ++ (x:xs) -- n:(x:xs) or n:x:xs
                | otherwise = x: (insRec n xs)

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insRec x (isort xs) --magic