isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

main = print $ maximum $ filter (isPalindrome . show) [x * y | x <- [100..999], y <- [x..999]]
