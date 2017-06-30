{-
Fibonacci (https://www.hackerrank.com/challenges/fibonacci-fp)
Memoization and DP, Easy, 30 pts

For each test case, print Fib_n % (10^8+7).

Constraints:
  1 <= T <= 104
  0 <= n <= 104
-}

import Data.Function.Memoize

fib :: Integer -> Integer
fib = memoize fib'
  where
    fib' 0 = 0
    fib' 1 = 1
    fib' n = (fib (n-1) + fib (n-2)) `mod` 100000007

main :: IO ()
main = do
  _ <- getLine
  r <- fmap (unlines . map (show . fib . read) . lines) getContents
  putStr r
