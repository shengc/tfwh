module Chapter3.BinarySearchFloor (bsFloor) where

type Interval = (Integer, Integer)

bsFloor :: Float -> Integer
bsFloor x = fst (until unit (shrink x) (bound x))
            where unit (m, n) = m + 1 == n

shrink :: Float -> Interval -> Interval
shrink x (m, n) = if fromInteger p <= x then (p, n) else (m, p)
                  where p = choose (m, n)

choose :: Interval -> Integer
choose (m, n) = (m + n) `div` 2

bound :: Float -> Interval
bound x = (lower x, upper x)

lower :: Float -> Integer
lower x = until ((<= x) . fromInteger) (*2) (-1)

upper :: Float -> Integer
upper x = until ((> x) . fromInteger) (*2) 1

