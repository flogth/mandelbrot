{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.Maybe (catMaybes)
import Data.Vector.Storable (fromList)
import Data.Word (Word8)
import Mandelbrot
import SDL

main :: IO ()
main = do
  initializeAll
  window <- createWindow "mandelbrot" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer

  dimensions <- get $ windowSize window

  appLoop renderer (fmap fromIntegral dimensions) defViewport

  destroyWindow window

data Viewport = Viewport
  { scale :: V2 Int,
    offset :: V2 Float
  }

defViewport :: Viewport
defViewport =
  Viewport
    { scale = V2 324 324,
      offset = V2 (-0.9) 0.05
    }

appLoop :: Renderer -> V2 Int -> Viewport -> IO ()
appLoop renderer dims vp@Viewport {scale = s, offset = o} = do
  drawBackground renderer backgroundColor
  events <- pollEvents
  let motions = map dispatchMotionEvent events
      nState = foldr updateViewport vp (catMaybes motions)

  let points = mandelbrotSet dims s o 64

  drawDots renderer foregroundColor points

  present renderer

  unless (checkExitEvent events) $ appLoop renderer dims nState

checkExitEvent :: [Event] -> Bool
checkExitEvent = any (\e -> isEscPress e || isExitEvent (eventPayload e))
  where
    isEscPress = keyPressed KeycodeEscape
    isExitEvent QuitEvent = True
    isExitEvent _ = False

keyPressed :: Keycode -> Event -> Bool
keyPressed keyCode event = case eventPayload event of
  KeyboardEvent keyevent -> pressEvent keyevent && isKeyCode keyevent
  _ -> False
  where
    pressEvent = (== Pressed) . keyboardEventKeyMotion
    isKeyCode = (== keyCode) . keysymKeycode . keyboardEventKeysym

data ViewControl = MoveLeft | MoveRight | MoveUp | MoveDown | ZoomIn | ZoomOut

dispatchMotionEvent :: Event -> Maybe ViewControl
dispatchMotionEvent e
  | keyPressed KeycodeLeft e = Just MoveLeft
  | keyPressed KeycodeRight e = Just MoveRight
  | keyPressed KeycodeUp e = Just MoveUp
  | keyPressed KeycodeDown e = Just MoveDown
  | keyPressed KeycodeY e = Just ZoomIn
  | keyPressed KeycodeX e = Just ZoomOut
  | otherwise = Nothing

updateViewport :: ViewControl -> Viewport -> Viewport
updateViewport w old@Viewport {scale = sc, offset = V2 x y} =
  case w of
    MoveLeft -> old {offset = V2 (x - step) y}
    MoveRight -> old {offset = V2 (x + step) y}
    MoveUp -> old {offset = V2 x (y - step)}
    MoveDown -> old {offset = V2 x (y + step)}
    ZoomIn -> old {scale = fmap (+ zoom) sc}
    ZoomOut -> old {scale = fmap (+ (- zoom)) sc}
  where
    step = 0.05
    zoom = 20

type Color = V4 Word8

backgroundColor :: Color
backgroundColor = V4 241 250 238 0

foregroundColor :: Color
foregroundColor = V4 29 53 87 0

drawBackground :: Renderer -> Color -> IO ()
drawBackground renderer color = do
  rendererDrawColor renderer $= color
  clear renderer

drawDots :: Renderer -> Color -> [V2 Int] -> IO ()
drawDots renderer color points = do
  rendererDrawColor renderer $= color
  drawPoints renderer $ fromList $ map (P . fmap fromIntegral) points
