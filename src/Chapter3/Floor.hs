module Chapter3.Floor (myFloor) where

myFloor :: Float -> Integer
myFloor x =
  if x < 0
     then until ((<= x) . fromInteger) (subtract 1) (-1)
     else until ((> x) . fromInteger) (+1) 1 - 1
