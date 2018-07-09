module Chapter5.Sudoku where

type Matrix a = [Row a]
type Row a = [a]

type Grid = Matrix Digit
type Digit = Char

digits :: [Digit]
digits = ['1'..'9']

blank :: Digit -> Bool
blank = (== '0')

solve :: Grid -> [Grid]
solve = filter valid . completions

completions :: Grid -> [Grid]
completions = expand . choices

choices :: Grid -> Matrix [Digit]
choices = map (map choice)
  where choice d = if blank d then digits else [d]

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- yss]
              where yss = cp xss

expand :: Matrix [Digit] -> [Grid]
expand = cp . map cp

valid :: Grid -> Bool
valid g = all nodups (rows g) && all nodups (cols g) && all nodups (boxes g)

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = notElem x xs && nodups xs

rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

boxes :: Matrix a -> Matrix a
boxes = map ungroup . ungroup . map cols . group 3. map (group 3)

group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs = take n xs : group n (drop n xs)

ungroup :: [[a]] -> [a]
ungroup = concat

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = map (remove fixed) row
  where fixed = [d | [d] <- row]
        remove :: [Digit] -> [Digit] -> [Digit]
        remove ds [x] = [x]
        remove ds xs = filter (`notElem` ds) xs

pruneBy :: (Matrix [Digit] -> Matrix [Digit]) -> Matrix [Digit] -> Matrix [Digit]
pruneBy f = f . map pruneRow . f

many :: (Eq a) => (a -> a) -> a -> a
many f = until (\x -> f x == x) f

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy boxes . pruneBy cols . pruneBy rows

solve2 :: Grid -> [Grid]
solve2 = filter valid . expand . many prune . choices

expand1' :: Matrix [Digit] -> [Matrix [Digit]]
expand1' rows
  = [rows1 ++ [row1 ++ [c]:row2] ++ rows2 | c <- cs ]
    where
      (rows1, row:rows2) = break (any (not . single)) rows
      (row1, cs:row2)    = span single row

single :: [a] -> Bool
single [_] = True
single _   = False

expand1 :: Matrix [Digit] -> [Matrix [Digit]]
expand1 rows
  = [rows1 ++ [row1 ++ [c]:row2] ++ rows2 | c <- cs]
    where
      (rows1, row:rows2) = break (any smallest) rows
      (row1, cs:row2)    = break smallest row
      smallest cs        = length cs == n
      n                  = minimum (counts rows)
      counts             = filter (/= 1) . map length . concat

complete :: Matrix [Digit] -> Bool
complete = all (all single)

safe :: Matrix [Digit] -> Bool
safe m = all ok (rows m) && all ok (cols m) && all ok (boxes m)
         where ok row = nodups [x | [x] <- row]

extract :: Matrix [Digit] -> Grid
extract = map (map head)

solve3 :: Grid -> [Grid]
solve3 = search . choices
search cm
  | not (safe pm) = []
  | complete pm = [extract pm]
  | otherwise = concatMap search (expand1 pm)
  where pm = prune cm
