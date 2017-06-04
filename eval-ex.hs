{-
Evaluating e^x (https://www.hackerrank.com/challenges/eval-ex)
FP/Introduction, Easy, 20 pts

The series expansion of e^x is given by:

  1 + x + x^2/2! + x^3/3! + x^4/4! + ...

Evaluate e^x for given values of x by using the above expansion for the first 10 terms.

Input: N, then N lines with values for x.
Output: N lines with evaluated e^x, for each x from input.
-}

import Control.Monad
import Text.Printf

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

pow :: (Num a, Integral b) => a -> b -> a
pow _ 0 = 1
pow x n = x * pow x (n - 1)

--exp10 :: a -> a
exp10 x = sum [pow x i / fromIntegral (factorial i) | i <- [0..9]]

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    forM_ [1..n] $ \a0  -> do
        x_temp <- getLine
        let x = read x_temp :: Double
        printf "%.4f\n" (exp10 x)
