{-# LANGUAGE NamedFieldPuns #-}

module Zambini where

import Data.Complex
import Data.Fixed (mod')

import Graphics.Gloss.Interface.Pure.Simulate

import ViewUtil

type R = Double
type C = Complex Double

data Zambini = Zambini { m :: R -- physical attributes
                       , i :: R
                       , r :: R
                       , fMax :: R
                       -- input
                       , f1 :: R
                       , f2 :: R
                       -- state
                       , x :: C
                       , v :: C
                       , theta :: R
                       , omega :: R
                       }

step :: R -> Zambini -> Zambini
step dt c@Zambini{m, i, r, fMax, f1, f2, x, v, theta, omega} = c{x=x', v=v', theta=theta', omega=omega'}
  where
    f1' = min 1 f1
    f2' = min 1 f2
    a = mkPolar ((f1' + f2') / m) theta
    v' = v + a * realToFrac dt
    x' = x + v' * realToFrac dt
    torque = r * (f1' - f2')
    omega' = omega + torque / i * dt
    theta' = theta + omega' * dt `mod'` 2*pi


render :: Zambini -> Picture
render Zambini{f1, f2, x, theta} = move x $ rotateRad (realToFrac theta) $ pictures [zambiniPic, control1 f1, control2 f2]
  where
    zambiniPic = color (greyN 0.5) $ pictures [ polygon [(-5,0), (-10, 25), (10, 25), (5, 0), (10, -25), (-10, -25)]
                                              , polygon [(0, 10), (20, 0), (0, -10)]
                                              ]

    control = rotate 180 . arrow (5, 20) (15, 100) (low, high)
      where
        low  = makeColorI 0 64 192 255
        high = makeColorI 0 160 192 255

    control1 = translate (-20) (-20) . control
    control2 = translate (-20) (20) . control

    move (x :+ y) = translate (realToFrac x) (realToFrac y)