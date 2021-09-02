-- In Haskell, function is a mapping that may take multiple arguments and returns a single result. Haskell is a pure language, that means it's functions are side-effect free.

double :: Int -> Int
double x = x + x

{- Question 1- 
 -
 - double (double 2)       { appliying outer double }
 - (double 2) + (double 2) { applying first double }
 - (2 + 2) + (double 2)    { applying second double }
 - (2 + 2) + (2 + 2)       { applying (+) functions from left to right} 
 - 8
 - -}

{- Question 2
 - sum defined as:
 - sum [] = 0
 - sum (n:ns) = n + sum ns
 -
 - sum [x] = x + sum [] { sum [] = 0}
 - sum [x] = x + 0
 - sum [x] = x
 - -}

-- Question 3
product' :: [Int] -> Int
product' [] = 1
product' (n:ns) = n * product' ns

-- Question 4
rqsort :: Ord a => [a] -> [a]
rqsort [] = []
rqsort (n:ns) = rqsort bigger ++ [n] ++ rqsort smaller
                where
                    smaller = [a | a <- ns, a <= n]
                    bigger  = [b | b <- ns, b > n]

-- Question 5: result will be set of values
qsortset :: Ord a => [a] -> [a]
qsortset [] = []
qsortset (n:ns) = qsortset smaller ++ [n] ++ qsortset bigger
                  where
                      smaller = [a | a <- ns, a < n]
                      bigger  = [b | b <- ns, b > n]

