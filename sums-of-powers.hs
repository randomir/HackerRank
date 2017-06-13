{-
The Sums of Powers (https://www.hackerrank.com/challenges/functional-programming-the-sums-of-powers)
FP/Recusion, Easy, 20 pts

Find the number of ways that a given integer, X, can be expressed as
the sum of the Nth power of unique, natural numbers.
-}

sums' :: Int -> Int -> Int
sums' 0 _ = 0
sums' x n = sums x maxk
  where
    maxk = round $ fromIntegral x ** recip (fromIntegral n)
    sums 0 _ = 1
    sums x k = sum [sums (x-p) (i-1) | i <- [k,k-1..1], let p = i^n, p <= x]

main :: IO ()
main = do
  t <- getContents
  let [x,n] = map read $ lines t
  print $ sums' x n


{-- for debugging --

-- a list of all partitions of x to naturals powered to n
partitions' :: Int -> Int -> [[Int]]
partitions' x n = snd $ partitions x maxk n
  where
    maxk = round $ fromIntegral x ** recip (fromIntegral n)

-- returns all partitions of x to naturals powered to n, starting with k
-- bool prefix is a flag that signifies if a partition is possible; if yes, second elem is the list of partitions
partitions :: Int -> Int -> Int -> (Bool, [[Int]])
partitions 1 1 n = (True, [[1]])
partitions 0 _ n = (True, [[]])
partitions _ 1 n = (False, [])
partitions _ 0 n = (False, [])
partitions x k n = (True, concat [map (i:) ss | i <- [k,k-1..1], let p = i^n, p <= x, let (ok,ss) = partitions (x-p) (i-1) n, ok])

-}
