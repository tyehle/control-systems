{-# LANGUAGE NamedFieldPuns #-}

module Boat where

import Data.Complex
import Data.Fixed (mod')

type R = Double
type C = Complex Double

data Boat = Boat { m :: R
                 , i :: R
                 , r :: R
                 , fMax :: R
                 , x :: C
                 , v :: C
                 , theta :: R
                 , omega :: R
                 }

type Input = (R, R)

step :: Input -> R -> Boat -> Boat
step (f1, f2) dt c@Boat{m, i, r, fMax, x, v, theta, omega} = c{x=x', v=v', theta=theta', omega=omega'}
  where
    a = mkPolar ((f1 + f2) / m) theta
    v' = v + a * realToFrac dt
    x' = x + v' * realToFrac dt
    torque = r * (f1 - f2)
    omega' = omega + torque / i * dt
    theta' = theta + omega' * dt `mod'` 2*pi
