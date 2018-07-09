module Chapter5.Exercise where

import Chapter5.Sudoku
import Data.List (sort)

add1 :: Matrix Integer -> Matrix Integer
add1 = map (map (+1))

sumUp :: Num a => Matrix a -> a
sumUp = sum . map sum

addM :: Num a => Matrix a -> Matrix a -> Matrix a
addM = zipWith (zipWith (+))

dotM :: Num a => Matrix a -> Matrix a -> Matrix a
dotM xs ys =
  let cs = cols ys
      mul x y = sum (zipWith (*) x y)
  in  group (length cs)  [mul x c | x <- xs, c <- cs]

transpose :: [[a]] -> [[a]]
transpose [] = repeat []
transpose (xs:xss) = zipWith (:) xs (transpose xss)

transpose' :: [[a]] -> [[a]]
transpose' xss
  | all null xss = []
  | otherwise = map head xss : transpose' (map tail xss)

-- any p = not . all (not p) -- this is true
-- any null = null . cp -- this is true too

inodups :: (Ord a) => [a] -> Bool
inodups xs = all (uncurry (/=)) (zip ys (tail ys))
             where ys = sort xs

nub :: (Ord a) => [a] -> [a]
nub [] = []
nub xs = head ys : nub (dropWhile (== head ys) ys)
         where ys = sort xs

ispan :: (a -> Bool) -> [a] -> ([a], [a])
ispan p xs = until pred go ([], xs)
  where pred (_, []) = True
        pred (_, y:_) = not (p y)
        go (xs, []) = (xs, [])
        go (xs, y:ys) = (xs ++ [y], ys)

whiteSpace :: Char -> Bool
whiteSpace ' ' = True
whiteSpace '\t' = True
whiteSpace '\n' = True
whiteSpace _ = False

iwords :: String -> [String]
iwords [] = []
iwords text =
  let (word, rest) = break whiteSpace text
      (_, remaining) = span whiteSpace rest
  in word : iwords remaining

iminimum :: Ord a => [a] -> a
iminimum [x] = x
iminimum (x:y:ys) = iminimum (min x y:ys)
