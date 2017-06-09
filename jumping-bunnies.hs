{-
Jumping Bunnies (https://www.hackerrank.com/challenges/jumping-bunnies)
FP/Ad Hoc, Medium, 20 pts

Each bunny has a jump length, find the nearest point where all bunnies can meet.
-}
multiple :: [Int] -> Int
multiple = foldl1 lcm

main :: IO()
main = do
  _ <- getLine
  l <- getLine
  let js = map (read::String->Int) $ words l
  print $ multiple js
