-- conditionals, guarded equations, pattern matching, lambda expressions

-- functions can be constructed by using already existing functions
split :: Int -> [a] -> ([a], [a])
split n xs = (take n xs, drop n xs)

double :: Int -> Int
double n = n + n

quadruple :: Int -> Int
quadruple n = double ( double n )

-- Conditionals
isAnyBiggerThan :: Ord a => a -> [a] -> Bool
isAnyBiggerThan t [] = False
isAnyBiggerThan t (x:xs) = if t > x then True else isAnyBiggerThan t xs

-- halve, assume argument has even number of elements
halve :: [a] -> ([a],[a])
halve xs = (take a xs, drop a xs)
            where
              a = (length xs) `div` 2

thirdHT, thirdLI, thirdPM :: [a] -> a
thirdHT xs = head (tail (tail xs))
thirdLI xs = xs !! 2
thirdPM (_:_:n:_) = n

safetail :: [a] -> Maybe [a]
safetail [] = Nothing
safetail (x:xs) = Just xs

safetailCE, safetailGE, safetailPM :: Eq a => [a] -> [a]
safetailCE xs = if null xs then [] else tail xs
safetailGE xs | null xs = []
              | otherwise = tail xs
safetailPM [] = []
safetailPM (x:xs) = xs

(||) :: Bool -> Bool -> Bool
b || c | b == c = b
       | otherwise = True

(|||) :: Bool -> Bool -> Bool
False ||| False = False
_ ||| _ = True

(||||) :: Bool -> Bool -> Bool
False |||| b = b
b |||| True = True

(|||||) :: Bool -> Bool -> Bool
False ||||| False = False
False ||||| True = True
True ||||| False = True
True ||||| True = True

(&&) :: Bool -> Bool -> Bool
(&&) b d = if (b == d) then b else False

(&&&) :: Bool -> Bool -> Bool
(&&&) b d = if (b == False) then False else
              if (d == True) then True else False

mult :: Int -> Int -> Int -> Int
mult = (\x -> \y -> \z -> x*y*z)

luhnDouble :: Int -> Int
luhnDouble n = (n * 2) `mod` 9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z t = (luhnDouble x + y + luhnDouble z + t) `mod` 10 == 0