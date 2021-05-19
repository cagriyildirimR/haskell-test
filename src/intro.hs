import Prelude hiding (product)

double :: Int -> Int
double x = x + x

double2 :: Int -> Int
double2 = (*2)

product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * product xs

productFoldr :: Num a => [a] -> a
--productFoldr = foldr (*) 1 -- product [1..4] = 1#(2#(3#(4#[])
productFoldr = foldr (\n ns -> n * ns) 1

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [a | a <- xs, a <= x]
                 larger  = [b | b <-xs, b > x]
{-
qsort in kotlin
fun <E: Comparable<E>> quickSort(l: List<E>): List<E> {
    if (l.isEmpty()) return emptyList()
    return quickSort( l.drop(1).filter{ it <= l[0] } ) + l[0] + quickSort(l.drop(1).filter{ it > l[0] })
}
-}

-- effect of <= to < is returning sorted set

reverseQsort :: Ord a => [a] -> [a]
reverseQsort [] = []
reverseQsort (x:xs) = reverseQsort larger ++ [x] ++ reverseQsort smaller
                      where
                        smaller = [a | a <- xs, a <= x]
                        larger  = [b | b <-xs, b > x]
