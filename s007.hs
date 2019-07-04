divides :: Integral a => a -> a -> Bool
n `divides` m = m `mod` n == 0


floorSqrt :: (Integral a) => a -> a
floorSqrt = floor . sqrt . fromIntegral

isPrime :: Integral a => a -> Bool
isPrime n
    | n <= 1    = False
    | otherwise = not $ any (`divides` n) [2..(floorSqrt n)]

primes :: Integral a => [a]
primes = filter isPrime [1..]

main = print $ primes !! 10000