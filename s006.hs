numbers = [1..100]

main = print $ sum numbers ^ 2 - sum (map (^2) numbers)