module Chapter4.Triads (triads, triads2) where

divisors :: Int -> [Int]
divisors x = [d | d <- [2..x-1], x `mod` d == 0]

coprime :: Int -> Int -> Bool
coprime x y = disjoint (divisors x) (divisors y)

disjoint :: [Int] -> [Int] -> Bool
disjoint xs ys = null $ dropWhile id [x /= y | x <- xs, y <- ys ]

triads :: Int -> [(Int, Int, Int)]
triads n = [(x,y,z) | x <- [1..n], y <- [x+1..n],
                      coprime x y,
                      z <- [y+1..n], x*x+y*y==z*z]

triads2 :: Int -> [(Int, Int, Int)]
triads2 n = [(x,y,z) | x <- [1..m], y <- [x+1..n],
                       coprime x y,
                       z <- [y+1..n], x*x+y*y==z*z]
            where m = floor (fromIntegral n / sqrt 2)
