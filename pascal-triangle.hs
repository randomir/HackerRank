{-
Pascal's Triangle (https://www.hackerrank.com/challenges/pascals-triangle/problem)
FP/Recursion, Easy, 10 pts

For a given integer K, print the first K rows of Pascal's Triangle. Print each
row with each value separated by a single space. The value at the n-th row and
r-th column of the triangle is equal to n!/(r!*(n-r)!) where indexing starts
from 0. These values are the binomial coefficients.
-}
import Data.List

pascalSucc :: [Int] -> [Int]
pascalSucc xs = [1] ++ zipWith (+) xs (tail xs) ++ [1]

pascal :: Int -> [[Int]]
pascal n = take n $ iterate pascalSucc [1]

main :: IO()
main = do
  input <- getLine
  let t = pascal $ (read :: String -> Int) input
  mapM_ (putStrLn . foldl1 (++) . intersperse " " . map show) t
