{-
List Replication (https://www.hackerrank.com/challenges/fp-list-replication)
FP/Introduction, Easy, 10 pts

Given a list, repeat each element in the list n amount of times.
-}

f :: Int -> [Int] -> [Int]
f n arr = [x | x <- arr, i <- [1..n]]

main :: IO ()
main = getContents >>=
       mapM_ print. (\(n:arr) -> f n arr). map read. words
