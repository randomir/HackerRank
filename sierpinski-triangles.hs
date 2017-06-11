{-
Functions and Fractals: Sierpinski triangles
(https://www.hackerrank.com/challenges/functions-and-fractals-sierpinski-triangles)

FP/Recursion, Advanced, 30 pts

Generate and print sierpinski triangle up to level 5 inside a
32 row x 63 columns matrix.

n <- [0..5]
n=0 => one triangle
n=4 => see below:

_______________________________1_______________________________
______________________________111______________________________
_____________________________1___1_____________________________
____________________________111_111____________________________
___________________________1_______1___________________________
__________________________111_____111__________________________
_________________________1___1___1___1_________________________
________________________111_111_111_111________________________
_______________________1_______________1_______________________
______________________111_____________111______________________
_____________________1___1___________1___1_____________________
____________________111_111_________111_111____________________
___________________1_______1_______1_______1___________________
__________________111_____111_____111_____111__________________
_________________1___1___1___1___1___1___1___1_________________
________________111_111_111_111_111_111_111_111________________
_______________1_______________________________1_______________
______________111_____________________________111______________
_____________1___1___________________________1___1_____________
____________111_111_________________________111_111____________
___________1_______1_______________________1_______1___________
__________111_____111_____________________111_____111__________
_________1___1___1___1___________________1___1___1___1_________
________111_111_111_111_________________111_111_111_111________
_______1_______________1_______________1_______________1_______
______111_____________111_____________111_____________111______
_____1___1___________1___1___________1___1___________1___1_____
____111_111_________111_111_________111_111_________111_111____
___1_______1_______1_______1_______1_______1_______1_______1___
__111_____111_____111_____111_____111_____111_____111_____111__
_1___1___1___1___1___1___1___1___1___1___1___1___1___1___1___1_
111_111_111_111_111_111_111_111_111_111_111_111_111_111_111_111
-}

width, height :: Int
width = 63
height = 32

data Pixel = Off | On
type Canvas = [[Pixel]]

instance Show Pixel where
  show Off = "_"
  show On = "1"

showCanvas :: Canvas -> String
showCanvas c = unlines [concatMap show r | r <- c]

empty :: Canvas
empty = replicate height (replicate width Off)

zero :: Canvas
zero = [replicate b Off ++ replicate w On ++ replicate b Off | r <- [0..height-1], let w = r*2+1, let b = div (width-w) 2]

{- generate subtriangles top-points on level n, inside triangle top-rooted on (r0,c0) -}
keypoints :: Int -> (Int,Int) -> [(Int,Int)]
keypoints n (r0,c0) = [(r0,c0), (r0+h,c0-h), (r0+h,c0+h)]
  where
    h = div height (2^n)

paintOne :: Canvas -> (Int,Int) -> Pixel -> Canvas
paintOne cc (r,c) p = take r cc ++ [take c row ++ [p] ++ drop (c+1) row] ++ drop (r+1) cc
  where row = cc !! r

{- paint black inverted triangle spanned by the two points given -}
paintDown :: Canvas -> (Int,Int) -> (Int,Int) -> Canvas
paintDown cc (r1,c1) (_,c2) = foldl poke cc [(r,c) | r <- [r1..r1+h-1], c <- [c1+(r-r1)+1..c2-(r-r1)-1]]
  where
    w = c2 - c1 - 1
    h = div (w + 1) 2
    poke canvas point = paintOne canvas point Off


generate :: Int -> Int -> Canvas -> (Int,Int) -> Canvas
generate i n cc (r,c)
  | i == n    = cc
  | otherwise =
    let
      [p0,p1,p2] = keypoints (i+1) (r,c)
      cc' = paintDown cc p1 p2
      subgen = generate (i+1) n
    in
      foldl subgen cc' [p0,p1,p2]

main :: IO ()
main = do
  n <- getLine
  putStrLn $ showCanvas $ generate 0 (read n) zero (0, div (width-1) 2)
