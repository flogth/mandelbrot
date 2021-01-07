module Mandelbrot (mandelbrotSet) where

import SDL (V2 (..))

type Complex = V2 Float

square :: Complex -> Complex
square (V2 a b) = V2 (a * a - b * b) (2 * a * b)

squaredLength :: Complex -> Float
squaredLength (V2 a b) = a * a + b * b

-- | z_0 = 0
--  z_{n+1} = z_n^2 + c
--  Divergiert ( z > threshold )          -> c nicht in Menge
--  Konvergiert bis maximal n Iterationen -> c in Menge
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
        zn = square z + c

mandelbrotSet :: V2 Int -> V2 Int -> Complex -> Int -> [V2 Int]
mandelbrotSet (V2 width height) (V2 rStep iStep) (V2 rShift iShift) maxIter =
  [n | a <- [0 .. width - 1], b <- [0 .. height - 1], let n = V2 a b, p n]
  where
    trans :: V2 Int -> Complex
    trans (V2 x y) =
      V2
        ((fromIntegral (x - rStep) / fromIntegral rStep) + rShift)
        ((fromIntegral (y - iStep) / fromIntegral iStep) + iShift)
    p = isInMandelbrot maxIter . trans
