n `divides` m = m `mod` n == 0

floorSqrt :: (Integral a) => a -> a
floorSqrt = floor . sqrt . fromIntegral

isPrime :: (Integral a) => a -> Bool
isPrime n
    | n <= 1    = False
    | otherwise = not $ any (`divides` n) [2..(floorSqrt n)]

primes :: (Integral a) => [a]
primes = filter isPrime [1..]

primeFactors :: (Integral a) => a -> [a]
primeFactors 1 = []
primeFactors n =
    let primeFactor = head (filter (`divides` n) primes)
    in primeFactor : primeFactors (n `div` primeFactor)


main = print $ maximum $ primeFactors 600851475143
