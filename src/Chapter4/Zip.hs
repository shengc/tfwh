module Chapter4.Zip where

nondec :: Ord a => [a] -> Bool
nondec xs = and (zipWith (<=) xs (tail xs))

position :: Eq a => a -> [a] -> Int
position x xs =
  head $ [j | (j, y) <- zip [0..] xs, y == x] ++ [-1]
