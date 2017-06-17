{-
Valid BST (https://www.hackerrank.com/challenges/valid-bst)
FP/Functional Structures, Medium, 20 pts

Given a list of numbers, determine whether it can represent the preorder
traversal of a binary search tree (BST).
-}

import Control.Monad

validTree :: [Int] -> Bool
validTree [] = True
validTree (x:xs) = validTree left && validTree right && rightOk
  where
    left = takeWhile (<=x) xs
    right = dropWhile (<=x) xs
    rightOk = if not $ null right then minimum right > x else True


main :: IO ()
main = do
  t <- fmap (read::String->Int) getLine
  forM_ [1..t] $ \_ -> do
    n <- fmap (read::String->Int) getLine
    a <- getLine
    let as = map (read::String->Int) $ words a
    putStrLn $ if validTree as then "YES" else "NO"
