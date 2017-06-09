{-
Remove Duplicates (https://www.hackerrank.com/challenges/remove-duplicates)
FP/Ad Hoc, Easy, 10 pts

You are given a string, str, of length N consisting of lowercase letters of
alphabet. You have to remove all those characters from str which have already
appeared in it, i.e., you have to keep only first occurance of each letter.
-}

nodups :: String -> String -> String
nodups [] _ = []
nodups (x:xs) v = if x `elem` v then nodups xs v else x : nodups xs (x:v)

main :: IO()
main = do
  str <- getLine
  putStrLn $ nodups str []
