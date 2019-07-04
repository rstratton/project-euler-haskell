import qualified Data.Map as Map

divides :: (Integral a) => a -> a -> Bool
n `divides` m = m `mod` n == 0

floorSqrt :: (Integral a) => a -> a
floorSqrt = floor . sqrt . fromIntegral

isPrime :: (Integral a) => a -> Bool
isPrime n
    | n <= 1    = False
    | otherwise = not $ any (`divides` n) [2..(floorSqrt n)]

primes :: (Integral a) => [a]
primes = filter isPrime [1..]

-- Get an array of prime factors
primeFactorsArray :: (Integral a) => a -> [a]
primeFactorsArray 1 = []
primeFactorsArray n =
    let primeFactor = head (filter (`divides` n) primes)
    in primeFactor : primeFactorsArray (n `div` primeFactor)

-- Increment the value associated with key `n` (or set to `1` if non-existent)
setOrInc :: Integral n => Map.Map n n -> n -> Map.Map n n
setOrInc map n = Map.insertWith (+) n 1 map

-- Get prime factorization as a map from factor to multiplicity
primeFactors :: Integral a => a -> Map.Map a a
primeFactors = foldl setOrInc Map.empty . primeFactorsArray

-- Merge two prime factorizations, keeping the larger of the two multiplicities
merge = Map.unionWith max

-- Given a prime factorization, return the composite number it represents
factorsToNum :: Integral a => Map.Map a a -> a
factorsToNum = Map.foldlWithKey (\n p m -> n * p^m) 1

main = print $ factorsToNum $ foldl merge Map.empty $ map primeFactors [1..20]