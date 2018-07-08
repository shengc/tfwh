module Chapter1.Exercise (anagrams, song) where

import Data.List (intercalate)
import Data.Char (toLower)

type MyWord = String

filterN :: Int -> [MyWord] -> [MyWord]
filterN n = filter (\x -> length x == n)

searchAnagrams :: MyWord -> [MyWord]
searchAnagrams word = undefined

showAnagrams :: MyWord -> String
showAnagrams word = word ++ ": " ++ intercalate "," (searchAnagrams word)

anagrams :: Int -> [MyWord] -> String
anagrams n = intercalate "\n" . map showAnagrams . filterN n

units, lunits :: [String]
units = ["One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine"]
lunits = (map . map) toLower units

song :: Int -> String
song = intercalate "\n" . songs

songs :: Int -> [String]
songs n
  | n == 0 = []
  | otherwise = intercalate "\n" [unwords [units !! (n - 1), manOrMan n, "went to a mow"], "Went to mow a meadow", intercalate ", " (soManyMen True n) ++ " and his dog", "Went to mow a meadow"] : songs (n - 1)
  where manOrMan :: Int -> String
        manOrMan n = if n == 1 then "man" else "men"
        soManyMen :: Bool -> Int -> [String]
        soManyMen b n
          | n == 0 = []
          | b = unwords [units !! (n - 1), manOrMan n] : soManyMen False (n - 1)
          | otherwise = unwords [lunits !! (n - 1), manOrMan n] : soManyMen False (n - 1)
