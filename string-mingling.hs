{-
String Mingling (https://www.hackerrank.com/challenges/string-mingling)
FP/Introduction, Easy, 10 pts

Mingle two strings, P=p1p2p3... and Q=q1q2q3.. into R=p1q1p2q2p3q3...
-}

mingle :: String -> String -> String
mingle [] [] = []
mingle (x:xs) (y:ys) = x : y : mingle xs ys

main :: IO()
main = do
  x <- getLine
  y <- getLine
  putStrLn $ mingle x y
