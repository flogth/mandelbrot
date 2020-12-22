{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Maybe (catMaybes)
import Data.Word
import Foreign.C.Types
import Mandelbrot
import SDL

data Color = Color
  { red :: Word8,
    green :: Word8,
    blue :: Word8
  }
  deriving (Show, Eq)

black :: Color
black =
  Color
    { red = 0,
      green = 0,
      blue = 0
    }

white :: Color
white =
  Color
    { red = 255,
      green = 255,
      blue = 255
    }

toVect :: Color -> V4 Word8
toVect Color {red = r, green = g, blue = b} = V4 r g b 0

main :: IO ()
main = do
  initializeAll
  window <- createWindow "mandelbrot" defaultWindow
  (V2 sx sy) <- get $ windowSize window
  mapM_ putStrLn $ show <$> [sx, sy]
  renderer <- createRenderer window (-1) defaultRenderer

  let scale = 1
  rendererScale renderer $= fromIntegral <$> V2 scale scale

  appLoop renderer (fromIntegral sx `div` scale, fromIntegral sy `div` scale) defState

  destroyWindow window

data State = State
  { scale :: (Int, Int),
    offset :: (Float, Float)
  }
  deriving (Show, Eq)

defState =
  State
    { scale = (1200, 1200),
      offset = (-0.6, 0)
    }

type WindowDim = (Int, Int)

appLoop :: Renderer -> WindowDim -> State -> IO ()
appLoop renderer (sx, sy) state@State {scale = s, offset = o} = do
  drawBackground renderer black
  events <- pollEvents
  let motions = dispatchMotionEvent <$> events
      nState = foldr updateState state (catMaybes motions)

  let ps = mSet (sx, sy) s o 63

  let State {offset = (x, y)} = nState

  forM_ ps (\(x, y) -> drawDot renderer white (fromIntegral x) (fromIntegral y))

  present renderer
  appLoop renderer (sx, sy) nState

data WindowControl = MoveLeft | MoveRight | MoveUp | MoveDown | ZoomIn | ZoomOut

dispatchMotionEvent :: Event -> Maybe WindowControl
dispatchMotionEvent e = case eventPayload e of
  KeyboardEvent keyboardevent ->
    if keyboardEventKeyMotion keyboardevent == Pressed
      then case keysymKeycode (keyboardEventKeysym keyboardevent) of
        KeycodeLeft -> Just MoveLeft
        KeycodeRight -> Just MoveRight
        KeycodeUp -> Just MoveUp
        KeycodeDown -> Just MoveDown
        KeycodeLShift -> Just ZoomIn
        KeycodeLCtrl -> Just ZoomOut
      else Nothing
  _ -> Nothing

updateState :: WindowControl -> State -> State
updateState w s@State {scale = (sx, sy), offset = (x, y)} =
  case w of
    MoveLeft -> s {offset = (x - step, y)}
    MoveRight -> s {offset = (x + step, y)}
    MoveUp -> s {offset = (x, y - step)}
    MoveDown -> s {offset = (x, y + step)}
    ZoomIn -> s {scale = (sx + zoom, sy + zoom)}
    ZoomOut -> s {scale = (sx - zoom, sy - zoom)}
  where
    step = 0.01
    zoom = 20

drawBackground :: Renderer -> Color -> IO ()
drawBackground renderer color = do
  rendererDrawColor renderer $= toVect color
  clear renderer

drawDot :: Renderer -> Color -> CInt -> CInt -> IO ()
drawDot renderer color x y = do
  rendererDrawColor renderer $= toVect color
  let p = P $ V2 x y
  drawPoint renderer p
