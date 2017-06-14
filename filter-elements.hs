{-
Filter Elements (https://www.hackerrank.com/challenges/filter-elements)
FP/Recursion, Easy, 10 pts

Given a list of N integers A = [a1, a2, ..., aN], you have to find those
integers which are repeated at least K times. In case no such element exists you
have to print -1.

If there are multiple elements in A which are repeated at least K times, then
print these elements ordered by their first occurrence in the list.

Let's say A = [4, 5, 2, 5, 4, 3, 1, 3, 4] and K = 2. Then the output is
  4 5 3
-}

import Data.List
import qualified Data.Map as Map
import Control.Monad

filterElements :: [Int] -> Int -> [Int]
filterElements as k = map snd $ sort $ map (\(v,(i,x)) -> (i,v)) $ Map.toList $ Map.filter (\(i,x) -> x >= k) m
  where
    add (i,x) (j,y) = (min i j, x+y)
    m = Map.fromListWith add (zip as $ zip [0..] (repeat 1))

main :: IO ()
main = do
  t <- fmap (read::String->Int) getLine
  forM_ [1..t] $ \_ -> do
    l <- getLine
    let [n,k] = map (read::String->Int) $ words l
    a <- getLine
    let as = map (read::String->Int) $ words a
    putStrLn $ let r = filterElements as k in if null r then "-1" else unwords $ map show r
