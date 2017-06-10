{-
Kundu And Bubble Wrap (https://www.hackerrank.com/challenges/kundu-and-bubble-wrap)
FP/Ad Hoc, Hard, 50 pts

Kundu has a Bubble Wrap and like all of us she likes popping it. The Bubble wrap
has dimensions NxM, i.e. it has N rows and each row has M cells which has a
bubble. Initially all bubbles in filled with air and can be popped.

What Kundu does is randomly picks one cell and tries to pop it, there might be a
case that the bubble Kundu selected is already popped. In that case he has to
ignore this. Both of these steps take 1 second of time. Tell the total expected
number of seconds in which Kundu would be able to pop them all.

Input:
Input contains a single line containing two space seperated integers, N M,
representing the dimension of Bubble wrap.

Output:
Output the required answer in one line. The answer will be considered correct,
if its absolute error doesn't exceed 10^-2.

Constraints:
1 ≤ N, M ≤ 1000
-}

{-
it comes down to the "Coupon collector's problem": https://en.wikipedia.org/wiki/Coupon_collector%27s_problem
what's the expected time to collect all n coupons? => E = n * Hn = n * (1 + 1/2 + 1/3 + ... + 1/n)
-}

import Text.Printf
import Data.Char

expected :: Double -> Double
expected n = n * sum (map recip [1..n])

trim :: String -> String
trim = dropWhile isSpace

pretty :: Double -> String
pretty n = trim $ printf "%10.6f" n

main :: IO()
main = do
  l <- getLine
  let [n,m] = map (read::String->Double) $ words l
  putStrLn $ pretty $ expected (n * m)
