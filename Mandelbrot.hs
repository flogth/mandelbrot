module Mandelbrot (mandelbrotSet) where

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

mandelbrotSet :: V2 Int -> V2 Int -> Complex -> Int -> [V2 Int]
mandelbrotSet (V2 width height) (V2 rStep iStep) (V2 rShift iShift) maxIter =
  [n | a <- [1 .. width], b <- [1 .. height], let n = V2 a b, p n]
  where
    trans :: V2 Int -> Complex
    trans (V2 x y) = V2 ((fromIntegral (x - rStep) / fromIntegral rStep) + rShift)
                        ((fromIntegral (y - iStep) / fromIntegral iStep) + iShift)
    p = isInMandelbrot maxIter . trans
