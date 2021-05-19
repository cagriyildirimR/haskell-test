import Data.Char
import Prelude hiding (replicate)
-- in math comprehension notation is used as {x^2 | x € {1..5}}
-- haskell uses similar notation [x^2 | x <- [1..5]] = [1,4,9,16,25]
--  [(x,y)| x <- [1..3], x > 10, y<- [4,5]] == [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]

cipher :: Int -> String -> String
cipher _ [] = [] -- how to put this into guarded equation ?
cipher n (s:ss) | s == ' ' = [' '] ++ cipher n ss
                | otherwise = [alphabet ((index s) + n)] ++ cipher n ss

decipher :: Int -> String -> String
decipher n ss = cipher (-n) ss

index :: Char -> Int
index c = head [i | (i, l) <- zip [0..25] ['a'..'z'], l == c]

alphabet :: Int -> Char
alphabet i = ['a'..'z'] !! (i `mod` 26)


-- sum [x^2 | x <- [1..100]]

grid :: Int -> Int -> [(Int, Int)]
grid n m = [(n',m') | n' <- [0..n], m' <- [0..m]]

square :: Int -> [(Int, Int)]
square n = grid n n

replicate :: Int -> a -> [a]
replicate n e = [e | _ <- [1..n]]

pythagoras :: Int -> [(Int, Int, Int)]
pythagoras n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

factors :: Int -> [Int]
factors n = [n' | n' <- [1..n-1], mod n n' == 0]

perfects :: Int -> [Int]
perfects n = [n' | n' <- [2..n], sum (factors n') == n']

-- concat [[(x,y)|y<-[3,4]]|x<-[1,2]]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positionsF x xs = find x (zip xs [0..])

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [ x * y | (x,y) <- zip xs ys]