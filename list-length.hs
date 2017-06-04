{-
List Length (https://www.hackerrank.com/challenges/fp-list-length)
FP/Introduction, Easy, 10 pts

Count the number of elements in an array without using count, size or length
operators (or their equivalents).
-}

len :: [a] -> Int
len = sum . map (\_ -> 1)
