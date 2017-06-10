{-
Common Divisors (https://www.hackerrank.com/challenges/common-divisors)
FP/Ad Hoc, Easy, 20 pts

Mario and Luigi earn points in their steps to save the Princess Peach from a
dragon. Let's denote Mario's points by M and Luigi's by L. Princess Peach is
wondering how many postive integers are there that are divisors to both numbers,
M and L. Help her find the answer.
-}

import Control.Monad

primes = sieve [2..] where sieve (p:xs) = p : sieve [x | x <- xs, rem x p > 0]

factors :: Int -> [Int]
factors 1 = [1]
factors n =
  let p = head [i | i <- primes' ++ [n], rem n i == 0]
  in p : factors (div n p)
  where primes' = takeWhile (<= (round $ sqrt $ fromIntegral n)) primes

combineFactors :: [(Int,Int)] -> [(Int,Int)]
combineFactors [] = []
combineFactors [x] = [x]
combineFactors ((x,xv):(y,yv):xs) =
  if x == y then
    combineFactors ((x,xv+yv):xs)
  else
    (x,xv) : combineFactors ((y,yv):xs)

factorsCombined :: Int -> [(Int,Int)]
factorsCombined n = combineFactors $ zip (filter (>1) $ factors n) (repeat 1)

commonFactors :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
commonFactors [] _ = []
commonFactors _ [] = []
commonFactors ((x,i):xs) ((y,j):ys)
  | x == y  = (x, min i j) : commonFactors xs ys
  | x < y   = commonFactors xs ((y,j):ys)
  | x > y   = commonFactors ((x,i):xs) ys

commonDivisors :: Int -> Int -> Int
commonDivisors x y =
  let
    fx = factorsCombined x
    fy = factorsCombined y
    fxy = commonFactors fx fy
  in
    product $ map ((+1) . snd) fxy

main :: IO ()
main = do
  t <- fmap (read::String->Int) getLine
  forM_ [1..t] $ \_ -> do
    l <- getLine
    let [x,y] = map (read::String->Int) $ words l
    print $ commonDivisors x y
