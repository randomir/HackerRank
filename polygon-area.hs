{-
Compute the Area of a Polygon
(https://www.hackerrank.com/challenges/lambda-march-compute-the-area-of-a-polygon)

FP/Introduction, Easy, 20 pts

You are given the cartesian coordinates of a set of points in a 2-D plane. When
traversed sequentially, these points form a Polygon, P, which is not
self-intersecting in nature. Can you compute the area of polygon P?

Input: N, then N lines with points given as space-separated integers
Output: perimeter correct up to one decimal place

Constraints:
- no 2 points are coincident, points are given counter-clockwise
- 4 <= N <= 1000
- 0 <= x,y <= 1000
-}

import Control.Monad
import Text.Printf

cross :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Float
cross (x0,y0) (x1,y1) (x2,y2) =
  let
    ax = fromIntegral (x1 - x0)
    bx = fromIntegral (x2 - x0)
    ay = fromIntegral (y1 - y0)
    by = fromIntegral (y2 - y0)
  in
    ax * by - ay * bx

area :: Float -> (Int, Int) -> [(Int, Int)] -> Float
area s _ [] = abs (s / 2)
area s z [_] = area s z []
area s z (a:b:xs) = area (s + cross z a b) z (b:xs)

main :: IO()
main = do
  n <- fmap (read::String->Int) getLine
  points <- forM [1..n] (\_ -> fmap ((\[a, b]->(a,b)).map (read::String->Int).words) getLine :: IO (Int, Int))
  printf "%.1f" $ area 0 (head points) (tail points)
