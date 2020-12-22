module Mandelbrot where

type Complex = (Float, Float)

type Point = (Int, Int)

square :: Complex -> Complex
square (a, b) = (a * a - b * b, 2 * a * b)

add :: Complex -> Complex -> Complex
add (a, b) (x, y) = (a + x, b + y)

squaredLength :: Complex -> Float
squaredLength (a, b) = a * a + b * b

inMandelbrot :: Int -> Complex -> Bool
inMandelbrot maxIter n = go (0, 0) 0
  where
    go :: Complex -> Int -> Bool
    go x i
      | i > maxIter = True
      | squaredLength xn > 4.0 = False
      | otherwise = go xn (i + 1)
      where
        xn = square x `add` n

mSet :: (Int, Int) -> (Int, Int) -> (Float, Float) -> Int -> [Point]
mSet (width, height) (stepX, stepY) (shiftX, shiftY) maxIter =
  [(a, b) | a <- [1 .. width], b <- [1 .. height], inMandelbrot maxIter (cent a stepX shiftX, cent b stepY shiftY)]
  where
    cent :: Int -> Int -> Float -> Float
    cent x mid shift = (fromIntegral (x - mid) / fromIntegral mid) + shift
