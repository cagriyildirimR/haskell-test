primes = sieve [2..]

sieve :: Integral a => [a] -> [a]
sieve (p:xs) = p : sieve [x | x <- xs, mod x p /= 0]