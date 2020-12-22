module Mandelbrot (mSet) where

import SDL (V2 (..))

type Complex = V2 Float

square :: Complex -> Complex
square (V2 a b) = V2 (a * a - b * b) (2 * a * b)

add :: Complex -> Complex -> Complex
add (V2 a b) (V2 x y) = V2 (a + x) (b + y)

squaredLength :: Complex -> Float
squaredLength (V2 a b) = a * a + b * b

-- | z_0 = 0
--  z_{n+1} = z_n^2 + c
--  Divergiert ( z > threshold )          -> nicht in Menge
--  Convergiert bis maximal n Iterationen -> in Menge
isInMandelbrot :: Int -> Complex -> Bool
isInMandelbrot maxIter c = go c 0
  where
    threshold = 4.0
    go :: Complex -> Int -> Bool
    go z i
      | i > maxIter = True
      | squaredLength zn > threshold = False
      | otherwise = go zn (i + 1)
      where
        zn = square z `add` c

mSet :: V2 Int -> V2 Int -> V2 Float -> Int -> [V2 Int]
mSet (V2 width height) (V2 stepX stepY) (V2 shiftX shiftY) maxIter =
  [V2 a b | a <- [1 .. width], b <- [1 .. height], p a b]
  where
    cent :: Int -> Int -> Float -> Float
    cent x mid shift = (fromIntegral (x - mid) / fromIntegral mid) + shift

    p a b = isInMandelbrot maxIter (V2 (cent a stepX shiftX) (cent b stepY shiftY))
