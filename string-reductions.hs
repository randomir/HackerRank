{-
String Reductions (https://www.hackerrank.com/challenges/string-reductions)
FP/Recursion, Easy, 10 pts

Given a string, str=s1s2s3...sn, consisting of n lowercase English characters
(a-z), remove all of the characters that occurred previously in the string.

=> duplicate of "remove-duplicates"
-}

reduce :: String -> String -> String
reduce [] _ = []
reduce (x:xs) v = if x `elem` v then reduce xs v else x : reduce xs (x:v)

main :: IO()
main = do
  str <- getLine
  putStrLn $ reduce str []
