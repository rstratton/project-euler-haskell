import qualified Data.Maybe as M

isPythag :: (Int, Int, Int) -> Bool
isPythag (a, b, c) = a^2 + b^2 == c^2

-- I thought I could enumerate all triplets with a list comprehension
-- but I couldn't find a way to do it with the added constraint that 
-- (a + b + c) == 1000.  I suspect there's a way to to it with a
-- list comprehension that I haven't figured out yet.
tripletSucc :: Maybe (Int, Int, Int) -> Maybe (Int, Int, Int)
tripletSucc Nothing = Nothing
tripletSucc (Just (332, 333, 334)) = Nothing
tripletSucc (Just (a, b, c))
    | b + 1 > c - 1 = Just (a + 1, a + 2, 997 - 2 * a)
    | otherwise     = Just (a, b + 1, c - 1)

triplets :: [Maybe (Int, Int, Int)]
triplets = Just (1, 2, 997) : map tripletSucc triplets

mulTriplet :: (Int, Int, Int) -> Int
mulTriplet (a, b, c) = a * b * c

main = print $ mulTriplet $ head $ filter isPythag $ map M.fromJust $ filter M.isJust triplets