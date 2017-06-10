{-
Missing Numbers (FP) (https://www.hackerrank.com/challenges/missing-numbers-fp)
FP/Ad Hoc, Easy, 30 pts

Sometimes you need to compare lists of number, but sorting each one normally
will take too much time. Instead you can use alternative methods to find the
differences between each list.

Challenge
Numeros The Artist was arranging two identical lists A and B into specific
orders. The arrangements of the two arrays were random, Numeros was very proud
of his arrangements. Unfortunately, some numbers got left out of List A. Can you
find the missing numbers from A without messing up his order?

Details
There are many duplicates in the lists, but you need to find the extra numbers,
i.e. B - A. Print the numbers in numerical order. Print each missing number
once, even if it is missing multiple times. The numbers are all within a range
of 100 from each other.

Input Format
There will be four lines of input:

n - the size of the first list
This is followed by n numbers that makes up the first list.
m - the size of the second list
This is followed by m numbers that makes up the second list.

Output Format
Output all the numbers (in ascending order) that are in B but not in A.

Constraints
1<= n,m <= 200000
-10000 <= x <= 10000 , x ∈ B
Xmax - Xmin < 101
-}

import qualified Data.Map as Map
import Data.List

listDiff :: [Int] -> [Int] -> [Int]
listDiff a b =
  sort $ map fst $ Map.toList $ Map.differenceWith combine bm am
  where
    am = Map.fromListWith (+) $ zip a $ repeat 1
    bm = Map.fromListWith (+) $ zip b $ repeat 1
    combine l r = if l == r then Nothing else Just (l - r)


main :: IO()
main = do
  getLine
  as <- getLine
  getLine
  bs <- getLine
  let a = map (read::String->Int) $ words as
  let b = map (read::String->Int) $ words bs
  putStrLn $ unwords $ map show $ listDiff a b
