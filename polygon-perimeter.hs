{-
Compute the Perimeter of a Polygon
(https://www.hackerrank.com/challenges/lambda-march-compute-the-perimeter-of-a-polygon)

FP/Introduction, Easy, 15 pts

You are given the cartesian coordinates of a set of points in a 2-D plane. When
traversed sequentially, these points form a Polygon, P, which is not
self-intersecting in nature. Can you compute the perimeter of polygon P?

Input: N, then N lines with points given as space-separated integers
Output: perimeter correct up to one decimal place

Constraints:
- no 2 points are coincident, points are given clockwise
- 3 <= N <= 1000
- 0 <= x,y <= 1000
-}

import Control.Monad
import Text.Printf

len :: (Int, Int) -> (Int, Int) -> Float
len (x1,y1) (x2,y2) =
  let
    dx = fromIntegral (x2 - x1)
    dy = fromIntegral (y2 - y1)
  in
    sqrt (dx ** 2 + dy ** 2)

perimeter :: Float -> [(Int, Int)] -> Float
perimeter s [] = s
perimeter s [_] = s
perimeter s (a:b:xs) = perimeter (s + len a b) (b:xs)

main :: IO()
main = do
  n <- fmap (read::String->Int) getLine
  points <- forM [1..n] (\_ -> fmap ((\[a, b]->(a,b)).map (read::String->Int).words) getLine :: IO (Int, Int))
  printf "%.1f" $ perimeter 0 $ points ++ [head points]
