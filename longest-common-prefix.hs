{-
Prefix Compression (https://www.hackerrank.com/challenges/prefix-compression/problem)
FP/Recursion, Easy, 10 pts

You are in charge of data transfer between two Data Centers. Each set of data is
represented by a pair of strings. Over a period of time you have observed a
trend: most of the times both strings share some prefix. You want to utilize
this observation to design a data compression algorithm which will be used to
reduce amount of data to be transferred.

You are given two strings, x and y, representing the data, you need to find the
longest common prefix (p) of the two strings. Then you will send substring p, x'
and y', where x' and y' are the substrings left after stripping p from them.
-}

prefix :: String -> String -> String
prefix [] _ = []
prefix _ [] = []
prefix (a:as) (b:bs) = if a == b then a : prefix as bs else []

out :: String -> IO ()
out x = putStrLn $ show (length x) ++ " " ++ x

main :: IO ()
main = do
  t <- getContents
  let
    [a,b] = lines t
    p = prefix a b
    pl = length p
  out p
  out (drop pl a)
  out (drop pl b)
