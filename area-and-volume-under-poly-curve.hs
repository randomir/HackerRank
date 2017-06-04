{-
Area Under Curves and Volume of Revolving a Curve
(https://www.hackerrank.com/challenges/area-under-curves-and-volume-of-revolving-a-curv)

FP/Introduction, Easy, 30 pts

Using equal subintervals of length = 0.001, you need to:
1. Evaluate the area bounded by a given polynomial function, between the given limits of L and R.
2. Evaluate the volume of the solid obtained by revolving this polynomial curve around the x-axis.
An absolute error margin of 0.02 will be tolerated.
-}

import Text.Printf (printf)

step = 0.001 :: Double
half = step / 2

f :: [Double] -> [Double] -> Double -> Double
f a b x = sum [ai * x ** bi | (ai,bi) <- zip a b]

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve il ir ia ib =
    let (as, vs) = unzip [let fi = f a b xi in (fi * step, fi ^^ 2 * pi * step) | xi <- [l+half,l+half+step..r]]
    in [sum as, sum vs]
    where
        l = fromIntegral il
        r = fromIntegral ir
        a = map fromIntegral ia
        b = map fromIntegral ib
        half = step / 2

{-
main :: IO()
main = print (solve 1 4 [1,2,3,4,5] [6,7,8,9,10])
-}

main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
