{-
Update List (https://www.hackerrank.com/challenges/fp-update-list)
FP/Introduction, Easy, 10 pts

Update the values of a list with their absolute values.
-}

f = map abs

main = do
   inputdata <- getContents
   mapM_ putStrLn $ map show $ f $ map (read :: String -> Int) $ lines inputdata
