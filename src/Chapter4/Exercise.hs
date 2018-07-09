module Chapter4.Exercise where

allPairs :: [(Int, Int)]
allPairs = [(x, d-x) | d <- [0..], x <- [0..d]]

-- only Eq should be needed
disjoint :: (Ord a) => [a] -> [a] -> Bool
disjoint xs ys = and [x /= y | x <- xs, y <- ys]

taxinumbers :: Int -> [(Int, Int, Int, Int)]
taxinumbers n = [(a, b, c, d) | a <- [1..n], c <- [a+1..n], d <- [c..n], b <- [a..n], a*a*a+b*b*b==c*c*c+d*d*d]

data List a = Nil | Snoc (List a) a

instance Show a => Show (List a) where
  show Nil = "Nil"
  show (Snoc xs x) = "Snoc (" ++ show xs ++ ") " ++ show x

shead :: List a -> a
shead (Snoc Nil x) = x
shead (Snoc xs _) = shead xs

slast :: List a -> a
slast (Snoc _ x) = x

toList :: [a] -> List a
toList = go . reverse
  where go :: [a] -> List a
        go [] = Nil
        go (x:xs) = Snoc (go xs) x

fromList :: List a -> [a]
fromList = reverse . go
  where go :: List a -> [a]
        go Nil = []
        go (Snoc xs x) = x:go xs

itake :: Int -> [a] -> [a]
itake _ [] = []
itake 0 _ = []
itake n (x:xs) = x:itake (n-1) xs

idrop :: Int -> [a] -> [a]
idrop _ [] = []
idrop 0 xs = xs
idrop n (x:xs) = idrop (n-1) xs

iSplitAt :: Int -> [a] -> ([a], [a])
iSplitAt = go []
  where go :: [a] -> Int -> [a] -> ([a], [a])
        go hd 0 tl = (hd, tl)
        go hd _ [] = (hd, [])
        go hd n (x:xs) = go (hd ++ [x]) (n-1) xs

-- unzip :: [(a, b)] -> ([a], [b])
-- cross :: (a -> c, b -> d) (a, b) = (c, d)

class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

instance Bifunctor (,) where
  bimap f g (a, b) = (f a, g b)

data MyEither a b = MyLeft a | MyRight b

instance Bifunctor MyEither where
  bimap f _ (MyLeft a) = MyLeft (f a)
  bimap _ g (MyRight b) = MyRight (g b)
