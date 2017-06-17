{-
Lists and GCD (https://www.hackerrank.com/challenges/lists-and-gcd)
FP/Functional Structures, Easy, 20 pts

Given a prime factorization of two integers, find theirs greatest common divisor
(GCD), outputting it factorized (as inputs).
-}

import qualified Data.Map as Map

groupPrimes :: [Int] -> [(Int,Int)]
groupPrimes [] = []
groupPrimes (p:k:xs) = (p,k) : groupPrimes xs

gcdPrimes :: [[Int]] -> [(Int,Int)]
gcdPrimes l = Map.toAscList $ foldl1 (Map.intersectionWith min) $ map (Map.fromList . groupPrimes) l

main :: IO ()
main = do
  _ <- getLine
  c <- getContents
  let l = map (map (read::String->Int) . words) $ lines c
  putStrLn $ unwords $ map show $ concatMap (\(p,k) -> [p,k]) $ gcdPrimes l
