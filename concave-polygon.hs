{-
Concave Polygon (https://www.hackerrank.com/challenges/lambda-march-concave-polygon)
FP/Recusion, Medium, 40 pts

You are given the cartesian coordinates of a set of points in a 2D plane (in no particular order).
Each of these points is a corner point of some Polygon, P, which is not self-intersecting in nature.
Can you determine whether or not P is a concave polygon?

Input Format

The first line contains an integer, N, denoting the number of points.
The N subsequent lines each contain 2 space-separated integers denoting the respective x and y coordinates of a point.

Constraints

3 <= N <= 1000
0 <= x,y <= 1000

Output Format

Print YES if P is a concave polygon; otherwise, print NO.
-}

import Data.List
import Data.Function

tau = 2*pi

bottomPoint :: [(Int, Int)] -> (Int, Int)
bottomPoint = minimumBy (compare `on` snd)

slope :: (Int, Int) -> (Int, Int) -> Double
slope (x1,y1) (x2,y2) = atan2 dy dx
  where
    dx = fromIntegral (x2 - x1)
    dy = fromIntegral (y2 - y1)

angle :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Double
angle a b c = fixup $ slope b c - slope b a
  where fixup x = let y = x + tau in if y > tau then y - tau else y

comparePoints :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Ordering
comparePoints o x y = case signum (slope o x - slope o y) of
  -1 -> LT
  0 -> (compare `on` fst) x y
  _ -> GT

sortPoints :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
sortPoints o = sortBy (comparePoints o)

isConcave :: [(Int, Int)] -> Bool
isConcave [] = False
isConcave [_] = False
isConcave [_,_] = False
isConcave ps = or $ zipWith3 concave p (rot p) (rot (rot p))
  where
    o = bottomPoint ps
    p = sortPoints o ps
    rot (x:xs) = xs ++ [x]
    concave a b c = angle a b c < pi

main :: IO ()
main = do
  getLine
  c <- getContents
  let ans = isConcave $ map ((\[x,y]->(read x, read y)) . words) $ lines c
  putStrLn $ if ans then "YES" else "NO"
