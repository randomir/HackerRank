{-
String Compression (https://www.hackerrank.com/challenges/string-compression)
FP/Recursion, Easy, 10 pts

Basically, implement Run-Length Encoding.
Compress "aaabaaaaccaaaaba" => "a3ba4c2a4ba".
-}

compress :: String -> String
compress str = compress' str []
  where
    encode [] = []
    encode [a] = [a]
    encode (a:as) = a : show (length as + 1)
    compress' [] as = encode as
    compress' (x:xs) [] = compress' xs [x]
    compress' (x:xs) (a:as) =
      if x == a then
        compress' xs (x:a:as)
      else
        encode (a:as) ++ compress' (x:xs) []

main :: IO ()
main = do
  str <- getLine
  putStrLn $ compress str
