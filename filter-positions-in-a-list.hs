{-
Filter Positions in a List (https://www.hackerrank.com/challenges/fp-filter-positions-in-a-list)
FP/Introduction, Easy, 10 pts

For a given list with N integers, return a new list removing the elements at odd
positions.
-}

f :: [Int] -> [Int]
f lst = [x | (x,i) <- zip lst [1..], even i]

main = do
   inputdata <- getContents
   mapM_ (putStrLn. show). f. map read. lines $ inputdata
