{-
Super Digit (https://www.hackerrank.com/challenges/super-digit)
FP/Recursion, Medium, 20 pts

Def:
super_digit(9875) = super_digit(9+8+7+5)
                  = super_digit(29)
                  = super_digit(2+9)
                  = super_digit(11)
                  = super_digit(1+1)
                  = super_digit(2)
                  = 2.

Input: n, k

Output: super_digit P, where P = n repeated k times

Constraints:
1 <= n <= 10^100000
1 <= k <= 10^5
-}

digitSum :: Integer -> Integer
digitSum 0 = 0
digitSum n = let (q,r) = quotRem n 10 in r + digitSum q

superDigit :: Integer -> Integer -> Integer
superDigit n 1
  | n <= 9 = n
  | otherwise = superDigit (digitSum n) 1
superDigit n k = superDigit (k * superDigit n 1) 1

main :: IO ()
main = do
  l <- getLine
  let [n,k] = map read $ words l
  print $ superDigit n k
