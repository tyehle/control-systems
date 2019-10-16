{-# LANGUAGE NamedFieldPuns #-}

module BoatSim where

import Data.Complex (Complex((:+)))
import Graphics.Gloss.Interface.Pure.Simulate

import Boat
import ViewUtil


---------- MAIN LOOP STUFF ----------


initial :: Boat
initial = Boat { m = 0.1
               , i = 10
               , r = 1
               , fMax = 30
               , x = 0
               , v = 0
               , theta = 0
               , omega = 0
               }

-- getControl :: a -> Boat -> a
-- getControl = undefined

render :: Boat -> Picture
render Boat{x, theta} = move x $ rotateRad (realToFrac theta) $ pictures [boatPic, control1 0.5]
  where
    boatPic = color (greyN 0.5) $ polygon [(-5,0), (-10, 25), (10, 25), (5, 0), (10, -25), (-10, -25)]

    control = rotate 180 . arrow (5, 20) (15, 100) (low, high)
      where
        low  = makeColorI 0 64 192 255
        high = makeColorI 0 160 192 255

    control1 = translate (-20) (-20) . control
    control2 = translate (-20) (20) . control

    move (x :+ y) = translate (realToFrac x) (realToFrac y)


mainStep :: ViewPort -> Float -> Boat -> Boat
mainStep _ deltaFloat = step (0.5, 0) (realToFrac deltaFloat)


boatMain :: IO ()
boatMain = simulate display background fps initial render mainStep
  where
    display = InWindow "Control Systems" (1200, 800) (100, 100)
    background = black
    fps = 60