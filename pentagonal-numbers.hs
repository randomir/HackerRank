{-
Pentagonal Numbers (https://www.hackerrank.com/challenges/pentagonal-numbers)
Memoization and DP, Easy, 30 pts

Pentagonal numbers are the number of dots that can be shown in a pentagonal
pattern of dots. Let's represent the nth pentagonal number by P(n). The
following figure depicts pentagonal patterns for n ∈ {1, 2, 3, 4, 5}.
Your task is to find the value of P(n) for a given n.
-}

import Data.Function.Memoize

dots :: Int -> Int
dots = memoize dots'
  where
    dots' 1 = 1
    dots' n = dots (n-1) + 3 * (n-1) + 1

main :: IO ()
main = do
  _ <- getLine
  r <- fmap (unlines . map (show . dots . read) . lines) getContents
  putStr r
