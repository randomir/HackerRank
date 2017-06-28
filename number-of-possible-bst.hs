{-
Number of Binary Search Tree (https://www.hackerrank.com/challenges/number-of-binary-search-tree)
Memoization and DP, Medium, 20 pts

You are given N nodes, each having unique value ranging from [1, N], how many
different binary search tree can be created using all of them.
-}

import Data.Function.Memoize

-- We could use `Int`, but only if was at least 38 bits, which we don't know for sure
count :: Integer -> Integer
count = memoize count'
  where
    count' 0 = 1
    count' n = sum [count (k-1) * count (n-k) | k <- [1..n]] `mod` 100000007

main :: IO ()
main = do
  _ <- getLine
  r <- fmap (unlines . map (show . count . read) . lines) getContents
  putStr r
