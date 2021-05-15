import Prelude hiding (map)
import Prelude hiding (filter)

-- HO function is a function that take a function as an input or returns another function

f1 :: Int -> Int -> Int -- currying function takes an argument and returns a function of that argument
f1 n m = n + m

f2 :: Int -> (Int -> Int)
f2 n m = n + m -- f1 = f2

f3 :: (Int -> Int -> Int) -> Int -> Int
f3 f n = f 10 n

-- Book example

double :: Int -> Int
double n = n + n

twice :: (Int -> Int) -> Int -> Int
twice f n = f (f n) -- twice double 10 -- (10 + 10) + (10 + 10)

-- using lambda twice (\n -> n * n) 2 = 16, (2*2) * (2*2)

-- we can construct new function with existing functions
quadruple :: Int -> Int
quadruple = twice (*2)

thrice :: (Int -> Int) -> Int -> Int
thrice f n = f ( f ( f n ) )

-- Processing list

map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs] -- map double [1..10] == [2,4,6,8,10,12,14,16,18,20]
-- using recursion
-- map f [] = []
-- map f (x:xs) = f x : map f xs


{- similar function can be implemented in kotlin as
fun <A, B> Collection<A>.map(f: (A)-> B): Collection<B> {
    val d = mutableListOf<B>()
    for (i in this) {
        d.add(f(i))
    }
    return d
}
-}

filter :: (a -> Bool) -> [a] -> [a]
filter f xs = [x | x <- xs, f x]

-- using recursion
--filter f [] = []
--filter f (x:xs) = if f x then x:filter f xs else filter xs
-- or
--filter f (x:xs) | f x = x:filter f xs
--                | otherwise = filter f xs

{- similar function can be implemented in kotlin as
fun <A> Collection<A>.filter(f: (A)-> Boolean): Collection<A> {
    val d = mutableListOf<A>()
    for (i in this) {
        if (f(i)) d.add(i)
    }
    return d
}
-}

--foldr foldl
-- f [] = v
-- f (x:xs) = x # f xs

-- sum, product :: Num a => [a] -> a
-- sum [] = []
-- sum (x:xs) = x + sum xs
-- product [] = 1
-- product (x:xs) = x + product xs

sum :: Num a => [a] -> a
sum = foldr (+) 0

product :: Num a => [a] -> a
product = foldr (*) 1