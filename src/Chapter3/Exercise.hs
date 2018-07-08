module Chapter3.Exercise where

import Data.Ord
import Chapter3.Nat

isubtract :: Num a => a -> a -> a
isubtract = flip (-)

ipower :: (Fractional a, Integral b) => a -> b -> a
ipower m n
  | n == 0 = 1
  | even n = let x = ipower m (n `div` 2) in x * x
  | n > 0 = let x = ipower m ((n - 1) `div` 2) in m * x * x
  | n < 0 = let x = ipower m ((n + 1) `div` 2) in (1 / m) * x * x

idiv :: Integral a => a -> a -> a
idiv x y = floor (fromIntegral x / fromIntegral y)

isqrt :: Float -> Integer
isqrt x = fst $ until unit shrink (1, fromIntegral . floor $ x)
  where unit :: (Integer, Integer) -> Bool
        unit (m, n) = m + 1 == n
        shrink :: (Integer, Integer) -> (Integer, Integer)
        shrink (m, n) =
          let p = (m + n) `div` 2
          in if fromIntegral (p * p) <= x then (p, n) else (m, p)

isqrt2 :: Float -> Float
isqrt2 x = until unit go (x / 2)
  where unit :: Float -> Bool
        unit y = abs ( y * y - x ) < 0.0001 * x
        go :: Float -> Float
        go y = y - (y * y - x) / (2 * y)

instance Ord Nat where
  compare Zero Zero = EQ
  compare _ Zero = GT
  compare Zero _ = LT
  compare (Succ m) (Succ n) = compare m n

idivMod :: Nat -> Nat -> (Nat, Nat)
idivMod _ Zero = error "division on zero"
idivMod Zero _ = (Zero, Zero)
idivMod x y = until unit f (Zero, x)
  where unit :: (Nat, Nat) -> Bool
        unit (_, x) = x < y
        f :: (Nat, Nat) -> (Nat, Nat)
        f (m, x) =(Succ m, x - y)
