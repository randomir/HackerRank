{-
Hello World N Times (https://www.hackerrank.com/challenges/fp-hello-world-n-times)
FP/Introduction, Easy, 5 pts

Print "Hello World" N amount of times.
-}

import Control.Applicative
import Control.Monad
import System.IO

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    let line = foldr (\x v -> x ++ "\n" ++ v) "" ["Hello World" | _ <- [1..n]]
    putStr line
