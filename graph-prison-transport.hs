{-
Prison Transport (https://www.hackerrank.com/challenges/prison-transport)
FP/Functional Structures, Medium, 30 pts

There are N inmates numbered between [1, N] in a prison. These inmates have
superhuman strength because they have drunk a special concoction made by Dr.
Evil. They have to be transported by some buses to a new facility. But they are
bound by special chains which are made from strong carbon fibres. Each inmate is
either chained alone or is chained in a group along with one or more inmates. A
group of inmates are those who are directly or indirectly connected to each
other. Only one group can be transported per bus.

There are buses which will charge fixed amount bucks for transferring inmates.
Charges are directly proportional to the capacity of bus. If a bus charge K
bucks then it can carry upto K2 inmates at one time. Buses are available for all
positive integral cost ranging from [1, 2, 3, ...]. A bus can be used multiple
times, and each time it will charge. Note that a bus can also transfer less
number of inmates than it's capacity.

Find the minimal cost to transport all the inmates.

Input
The first line contains N representing the number of inmates. Second line
contains another integer, M, number of pairs of inmates who are handcuffed
together. Then follows M lines. Each of these lines contains two integers, P Q,
which means inmate numbered P is handcuffed to inmate numbered Q.

Output
For the given arrangement, print the minimal cost which can be incurred while
transferring inmates.

Constraints
2 ≤ N ≤ 100000
1 ≤ M ≤ min(N*(N-1)/2, 100000)
1 ≤ P, Q ≤ N
P ≠ Q
-}

{-
Solution: we need to find all connected subgraphs and sum up their sizes.
-}

import Data.Graph
import Data.Tree

groupSizes :: Int -> [(Int,Int)] -> [Int]
groupSizes n es = map (length . flatten) (components $ buildG (1,n) es)

main :: IO ()
main = do
  n <- readLn :: IO Int
  _ <- getLine
  content <- getContents
  let es = map ((\[a,b] -> (a,b)) . map read . words) (lines content)
  let bs = map (ceiling . sqrt . fromIntegral) $ groupSizes n es
  print $ sum bs
