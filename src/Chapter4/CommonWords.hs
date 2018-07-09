module Chapter4.CommonWords where

import Data.List (sort, sortBy)

showRuns :: (Int, String) -> String
showRuns (n, w) = w ++ ": " ++ show n ++ "\n"

countRuns :: [String] -> [(Int, String)]
countRuns [] = []
countRuns (w:ws) = (1 + length us, w):countRuns vs
                   where (us, vs) = span (==w) ws

sortWords :: [String] -> [String]
sortWords = sort

sortRuns :: [(Int, String)] -> [(Int, String)]
sortRuns = sortBy (flip compare)

isort :: Ord a => [a] -> [a]
isort [] = []
isort [x] = [x]
isort xs = merge (sort ys) (sort zs)
           where
             (ys, zs) = let n = length xs `div` 2 in (take n xs, drop n xs)
             merge :: (Ord a) => [a] -> [a] -> [a]
             merge [] ys = ys
             merge xs [] = xs
             merge xs'@(x:xs) ys'@(y:ys)
               | x <= y = x : merge xs ys'
               | otherwise = y : merge xs' ys

