{-
Convex Hull (https://www.hackerrank.com/challenges/convex-hull-fp)
FP/Recursion, Medium, 30 pts

Convex Hull of a set of points, in 2D plane, is a convex polygon with minimum
area such that each point lies either on the boundary of polygon or inside it.

Let's consider a 2D plane, where we plug pegs at the points mentioned.
We enclose all the pegs with a elastic band and then release it to take its
shape. The closed structure formed by elastic band is similar to that of convex
hull.

Input:
First line of input will contain a integer, N, number of points.
Then follow N lines where each line contains the coordinate, xi yi, of ith point.

Output:
Print the perimeter of convex hull for the given set of points. An error margin
of +/- 0.2 is acceptable.

Constraints:
3 <= N <= 104
0 <= xi, yi <= 104
There exists, at least, three points which are non-colinear.
-}

{-
We find convex hull with Graham's scan:
1. pick anchor point O, the one with the smallest y value (bottom-most)
2. sort all other points according to angle closing with the anchor point O
3. traverse points 1..N, adding one by one to the list of convex-hull points
  3.1. while last 3 points in cw direction, remove the penultimate point
-}

import Data.List
import Data.Function
import Text.Printf

ccw :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
ccw (x1,y1) (x2,y2) (x3,y3) = (x2-x1) * (y3-y1) - (y2-y1)*(x3-x1) > 0

reduce :: [(Int, Int)] -> [(Int, Int)]
reduce (a:b:c:h) = if ccw c b a then a:b:c:h else reduce (a:c:h)
reduce h = h

{- build convex hull, adding one point at the time -}
buildHull :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
buildHull h [] = h
buildHull h (p:ps) = buildHull (reduce (p:h)) ps

bottomPoint :: [(Int, Int)] -> (Int, Int)
bottomPoint = minimumBy (compare `on` snd)

slope :: (Int, Int) -> (Int, Int) -> Double
slope (x1,y1) (x2,y2) = atan2 dy dx
  where
    dx = fromIntegral (x2 - x1)
    dy = fromIntegral (y2 - y1)

comparePoints :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Ordering
comparePoints o x y = case signum (slope o x - slope o y) of
  -1 -> LT
  0 -> (compare `on` fst) x y
  _ -> GT

sortPoints :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
sortPoints o = sortBy (comparePoints o)

distance :: (Int, Int) -> (Int, Int) -> Double
distance (x1,y1) (x2,y2) = sqrt (dx ** 2 + dy ** 2)
  where
    dx = fromIntegral (x2 - x1)
    dy = fromIntegral (y2 - y1)

perimeter :: [(Int, Int)] -> Double
perimeter ps = sum $ zipWith distance ps (tail ps ++ ps)

solve :: [(Int, Int)] -> Double
solve points = perimeter $ buildHull [o] (sortPoints o points)
  where o = bottomPoint points

main :: IO ()
main = do
  n <- readLn :: IO Int
  content <- getContents
  let
    points = map (\[x, y] -> (x, y)). map (map (read::String->Int). words). lines $ content
    ans = solve points
  printf "%.1f\n" ans
