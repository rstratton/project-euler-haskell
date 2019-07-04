import qualified System.IO as IO
import qualified Data.Text as T
import qualified Data.Char as C

slice :: Int -> Int -> String -> String
slice a b string = T.unpack $ T.drop a $ T.take b $ T.pack string

sliceIndices :: [(Int, Int)]
sliceIndices = [(x, x+13) | x <- [0..]]

substrings :: String -> [String]
substrings string = takeWhile (\s -> length s == 13) [slice a b string | (a, b) <- sliceIndices]

stringToDigits :: String -> [Int]
stringToDigits = map (\c -> C.ord c - 48)

solution :: String -> Int
solution string = maximum [product $ stringToDigits s | s <- substrings string]

main = do
    contents <- readFile "s008.txt"
    print $ solution contents