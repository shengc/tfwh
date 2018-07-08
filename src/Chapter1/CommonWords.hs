module Chapter1.CommonWords where

import qualified Data.List as L
import qualified Data.Char as C

type Text = String
type MyWord = String

sortWords :: [MyWord] -> [MyWord]
sortWords = L.sort

countRuns :: [MyWord] -> [(Int, MyWord)]
countRuns = map (\xs -> (length xs, L.head xs)) . L.group

sortRuns :: [(Int, MyWord)] -> [(Int, MyWord)]
sortRuns = L.sortOn $ negate . fst

showRun :: (Int, MyWord) -> String
showRun (n, w) = w ++ ": " ++ show n

commonWords :: Int -> Text -> String
commonWords n = L.intercalate "\n" . map showRun . take n . sortRuns . countRuns . sortWords . words . map C.toLower
