{-
Fibonacci (https://www.hackerrank.com/challenges/fibonacci-fp)
Memoization and DP, Easy, 30 pts

For each test case, print Fib_n % (10^8+7).

Constraints:
  1 <= T <= 104
  0 <= n <= 104
-}

import Data.Function.Memoize


-- memoization with `memoize`, using a tree structure for faster lookups
fib :: Int -> Integer
fib = memoize fib'
  where
    fib' 0 = 0
    fib' 1 = 1
    fib' n = (fib (n-1) + fib (n-2)) `mod` 100000007


-- manual memoization using a linear structure for lookups (list)
fibl :: Int -> Integer
fibl = (map fib' [0..] !!)
  where
    fib' 0 = 0
    fib' 1 = 1
    fib' n = (fibl (n-1) + fibl (n-2)) `mod` 100000007


-- plain non-memoized fibonacci (slow)
fibs :: Int -> Integer
fibs 0 = 0
fibs 1 = 1
fibs n = (fibs (n-1) + fibs (n-2)) `mod` 100000007


main :: IO ()
main = do
  _ <- getLine
  r <- fmap (unlines . map (show . fib . read) . lines) getContents
  putStr r
