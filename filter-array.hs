{-
Filter Array (https://www.hackerrank.com/challenges/fp-filter-array)
FP/Introduction, Easy, 10 pts

Filter a given array of integers and output only those values that are less than
a specified value X. The output integers should be in the same sequence as they
were in the input.
-}

f :: Int -> [Int] -> [Int]
f n arr = [x | x <- arr, x < n]

main = do
    n <- readLn :: IO Int
    inputdata <- getContents
    let
        numbers = map read (lines inputdata) :: [Int]
    putStrLn . unlines $ (map show . f n) numbers
