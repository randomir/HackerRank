{-
Sum of Odd Elements (https://www.hackerrank.com/challenges/fp-sum-of-odd-elements)
FP/Introduction, Easy, 10 pts

You are given a list. Return the sum of odd elements from the given list. The
input and output portions will be handled automatically. You need to write a
function with the recommended method signature.
-}

f = sum . filter odd

main = do
   inputdata <- getContents
   putStrLn $ show $ f $ map (read :: String -> Int) $ lines inputdata
