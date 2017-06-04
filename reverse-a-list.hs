{-
Reverse a List (https://www.hackerrank.com/challenges/fp-reverse-a-list)
FP/Introduction, Easy, 10 pts

You are given a list of N elements. Reverse the list without using the reverse
function. The input and output portions will be handled automatically. You need
to write a function with the recommended method signature.
-}

rev [] = []
rev (x:xs) = rev xs ++ [x]
