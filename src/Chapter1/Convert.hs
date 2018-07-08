module Chapter1.Convert (convert) where

units, teens, tens :: [String]

units = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

teens = ["ten", "elevent", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

-- 0 <= n < 10
convert1 :: Int -> String
convert1 n = units !! n

-- 0 <= n < 100
convert2 :: Int -> String
convert2 n
  | t == 0 = convert1 u
  | t == 1 = teens !! u
  | u == 0 = tens !! (t - 2)
  | otherwise = tens !! (t - 2) ++ "-" ++ convert1 u
  where (t, u) = (n `div` 10, n `mod` 10)

-- 0 <= n < 1000
convert3 :: Int -> String
convert3 n
  | h == 0 = convert2 n
  | t == 0 = units !! h ++ " hundred"
  | otherwise = units !! h ++ " hundred and " ++ convert2 t
  where (h, t) = (n `div` 100, n `mod` 100)

convert6 :: Int -> String
convert6 n
  | m == 0 = convert3 h
  | h == 0 = convert3 m ++ " thousand"
  | otherwise = convert3 m ++ " thousand" ++ link h ++ convert3 h
  where (m, h) = (n `div` 1000, n `mod` 1000)

link :: Int -> String
link h
  | h < 100 = " and "
  | otherwise = " "

convert :: Int -> String
convert = convert6
