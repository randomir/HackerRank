{-
String-o-Permute (https://www.hackerrank.com/challenges/string-o-permute)
FP/Recursion, Easy, 10 pts

Swap the characters at the even positions with the next character. Indexing
starts at 0. Length of string will be even.
-}
import Control.Monad

permute :: String -> String
permute [] = []
permute (x:y:xs) = y : x : permute xs

main :: IO()
main = do
  t <- fmap (read :: String -> Int) getLine
  forM_ [1..t] $ \_ -> do
    s <- getLine
    putStrLn $ permute s
