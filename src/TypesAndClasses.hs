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

