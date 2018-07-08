module Chapter2.Exercise where

import qualified Data.Char as C

modernise :: String -> String
modernise = unwords . map (\(x : xs) -> C.toUpper x : map C.toLower xs) . words

first :: (a -> Bool) -> [a] -> a
first p xs | null xs = error "Empty list"
           | p (head xs) = head xs
           | otherwise = first p (tail xs)

firstOption :: (a -> Bool) -> [a] -> Maybe a
firstOption p = headOption . filter p
  where headOption :: [a] -> Maybe a
        headOption xs = if null xs then Nothing else Just (head xs)

myExp :: Integer -> Integer -> Integer
myExp x n
  | n == 0 = 1
  | n == 1 = x
  | even n = let y = myExp x (n `div` 2) in y * y
  | odd n  = let y = myExp x ((n - 1) `div` 2) in x * y * y

type Day = Int
type Month = Int
type Year = Int
type Date = (Day, Month, Year)
showDate :: Date -> String
showDate (d, m, y) = show d ++ ish d ++ " " ++ months !! (m - 1) ++ ", " ++ show y
  where ish :: Int -> String
        ish n =
          let m = n `mod` 10
          in if m == 1
                then "st"
                else if m == 2
                then "nd"
                else if m == 3
                then "rd"
                else "th"
        months :: [String]
        months = ["January", "Feburary", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

type CIN = String
addSum :: CIN -> CIN
addSum xs = xs ++ (show . sum $ map getDigit xs)
  where getDigit :: Char -> Int
        getDigit c = read [c]

valid :: CIN -> Bool
valid cin = length cin == 10 && addSum (take 8 cin) == cin

palindrome :: IO ()
palindrome =
  let isPalindrome :: String -> Bool
      isPalindrome line =
        let xs = map C.toLower $ filter C.isAlpha line
        in xs == reverse xs
  in do putStrLn "Enter a string:"
        line <- getLine
        putStrLn $ if isPalindrome line then "Yes!" else "No!"
