{-
Functions or Not? (https://www.hackerrank.com/challenges/functions-or-not)
FP/Introduction, Easy, 5 pts

You are given a set of unique (x,y) ordered pairs constituting a relation.
The x-values form the domain, and the y-values form the range to which they map.
For each of these relations, identify whether they may possibly represent a
valid function or not.

Note: You do not have to find the actual function, you just need to determine
that the relation may be representative of some valid function.
-}

import Control.Monad
import Data.List

validPair :: (Int,Int) -> (Int,Int) -> Bool
validPair (x1,y1) (x2,y2) = if x1 /= x2 then True else y1 == y2

testPairs a [] = a
testPairs a (x:[]) = a
testPairs a (x:y:res) = testPairs ((validPair x y):a) (y:res)

valid :: [(Int, Int)] -> Bool
valid f = and $ testPairs [] $ sort f

main = do
    t <- fmap (read::String->Int) getLine
    forM [1..t] (\_->do
        n <- fmap (read::String->Int) getLine
        func <- forM [1..n] (\_->do fmap ((\[a, b]->(a,b)).map (read::String->Int).words) getLine :: IO (Int, Int))
        putStrLn $ if valid func then "YES" else "NO")
