{-
Rotate String (https://www.hackerrank.com/challenges/rotate-string)
FP/Ad Hoc, Easy, 20 pts

Output all possible rotations of a string.
-}
import Control.Monad

rotate :: Int -> String -> [String]
rotate 0 _ = []
rotate n (x:xs) = rot : rotate (n-1) rot
                  where rot = xs ++ [x]

main :: IO()
main = do
  t <- fmap (read::String->Int) getLine
  forM_ [1..t] $ \_ -> do
    s <- getLine
    putStrLn $ unwords $ rotate (length s) s
