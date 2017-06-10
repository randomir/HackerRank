{-
Huge GCD (https://www.hackerrank.com/challenges/huge-gcd-fp)
FP/Ad Hoc, Easy, 30 pts

Gayasen has received a homework assignment to compute the greatest common divisor
of the two positive integers A and B. Since the numbers are quite large, the
professor provided him with N smaller integers whose product is A, and M
integers with product B. He would like to verify result, so he has asked you to
write a program to solve his problem. But instead of printing complete answer
you have to print answer modulo 109+7.

Input
First line of input contains the positive integer N (1 <= N <= 1000).
Second line of input contains N space-separated positive integers not greater than 104, whose product is the number A.
Third line of input contains the positive integer M (1 <= M <= 1000).
Fourth line of input contains M space-separated positive integers not greater than 104, whose product is the number B.

OUTPUT
Print the greatest common divisor of numbers A and B modulo 1000000007.

Constraints
1 <= N, M <= 1000
1 <= element of list <= 10000
-}

gcd' :: [Integer] -> [Integer] -> Integer
gcd' ns ms = (gcd (product ns) (product ms)) `mod` 1000000007

main :: IO()
main = do
  n <- getLine
  nl <- getLine
  m <- getLine
  ml <- getLine
  let ns = map (read::String->Integer) $ words nl
  let ms = map (read::String->Integer) $ words ml
  print $ gcd' ns ms
