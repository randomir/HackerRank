{-
Substring Searching (https://www.hackerrank.com/challenges/kmp-fp)
FP/Functional Structures, Medium, 50 pts

In 1974, a very fast string searching method was proposed by the name of KMP
algorithm with linear run-time complexity. Your task here is to code this (or
any similar) algorithm in a functional language.

Given two strings text and pat, find whether pat exists as a substring in text.

Input
First line will contain an integer, T, which represents total number of test
cases. Then T test cases follow. Each case will contains two lines each
containing a string. First line will contain text while the second line will
contain pat.

Output
For each case print YES if pat is a substring of text otherwise NO.

Constraints
1 ≤ T ≤ 10
1 ≤ |pat| ≤ |text| ≤ 100000
All characters in text and pat will be lowercase latin character ('a'-'z').
-}
import Control.Monad
import Data.Char
import Data.List

rk_q = 33554393
rk_d = 32

modulo :: Int -> Int
modulo = (`mod` rk_q)

-- hash as = (a_0 * d^(M-1) + a_1 * d^(M-2) + ... + a_M-2 * d + a_M-1) `mod` rk_q
hash :: String -> Int
hash [] = 0
hash as = foldl (\v c -> modulo (v*rk_d + ord c)) 0 as

-- Rabin-Karp substring search
contains :: String -> String -> Bool
contains text pattern = or $ map (\i -> pattern `isPrefixOf` (drop i text)) hits
  where
    len = length pattern
    dm = foldl (\a b -> modulo (a*b)) 1 $ replicate (len-1) rk_d
    hp = hash pattern
    ht0 = hash $ take len text
    rehash h (p,n) = modulo (modulo (h + rk_d*rk_q - ord p*dm) * rk_d + ord n)
    hs = scanl rehash ht0 $ zip text (drop len text)
    hits = map snd $ filter (\(h,i) -> h == hp) $ zip hs [0..]

main :: IO ()
main = do
  t <- fmap (read::String->Int) getLine
  forM_ [1..t] $ \_ -> do
    text <- getLine
    pattern <- getLine
    putStrLn $ if contains text pattern then "YES" else "NO"
