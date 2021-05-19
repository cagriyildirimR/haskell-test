-- [Char], (Char, Char, Char), [(Bool, Char)], ([Bool],[Char]), [[a]->[a]]
bools :: [Bool]
bools = [True, True, False]

nums :: [[Int]]
nums = [[1,2,3],[4,5]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a,a)
copy n = (n,n)

apply :: (String -> Int) -> String -> Int
apply f n = f n
-- apply (\n -> length n) "Hello World"


-- [a] -> a, (a,b)->(b,a), a -> b -> (a,b), Num a => a -> a, [a] -> Bool, (a -> b) -> a -> b
-- palindrome :: Eq a => [a] -> Bool
-- twice  (t -> t) -> t -> t

second xs = head(tail xs)
swap (x,y) = (y,x)
pair x y = (x,y)
double x = x*2
palindrome xs = reverse xs == xs
twice f x = f(f x)

--Note that function types are not in general instances of the Eq class,
-- because it is not feasible in general to compare two functions for equality

data Shape = Circle Float | Rect Float Float

instance Eq Shape where
  Circle a == Circle b = a == b

instance Ord Shape where
    Circle a <= Circle b = a <= b


-- 3 different approaches to declearing types

type XYZPos = (Int, Int, Int) -- creating new name for already existing type

moveRight :: XYZPos -> XYZPos
moveRight (x,y,z) = (x+1, y, z)

{-
type String = [Char]
-}

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']


safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (div n m)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:xs) = Just x

data Nat = Zero | Succ Nat deriving (Show)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

natadd :: Nat -> Nat -> Nat
natadd Zero n = n
natadd (Succ (n)) m = Succ (natadd n m)

f1, f2 :: Eq a => a -> a -> Bool
f1 n m = n == m
f2 n m = n /= m

data A = B | C | D deriving (Show)

instance Eq A where
  B == C = True
  D == C = True
  _ == _ = False

