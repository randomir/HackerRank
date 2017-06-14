{-
Sequence full of colors (https://www.hackerrank.com/challenges/sequence-full-of-colors)
FP/Recursion, Easy, 10 pts

You are given a sequence of N balls in 4 colors: red, green, yellow and blue.
The sequence is full of colors if and only if all of the following conditions are true:
- There are as many red balls as green balls.
- There are as many yellow balls as blue balls.
- Difference between the number of red balls and green balls in every prefix of the sequence is at most 1.
- Difference between the number of yellow balls and blue balls in every prefix of the sequence is at most 1.

Your task is to write a program, which for a given sequence prints True if it is
full of colors, otherwise it prints False.

4
RGGR
RYBG
RYRB
YGYGRBRB

True
True
False
False
-}

proper :: (Int,Int,Int,Int) -> String -> Bool
proper (r,g,b,y) [] = (r == g) && (y == b)
proper (r,g,b,y) (x:xs) = case x of
  'R' -> (abs r+1-g <= 1) && proper (r+1,g,b,y) xs
  'G' -> (abs g+1-r <= 1) && proper (r,g+1,b,y) xs
  'B' -> (abs b+1-y <= 1) && proper (r,g,b+1,y) xs
  'Y' -> (abs y+1-b <= 1) && proper (r,g,b,y+1) xs

main :: IO ()
main = do
  t <- getLine
  c <- getContents
  putStrLn $ unlines $ map (show . proper (0,0,0,0)) $ lines c
