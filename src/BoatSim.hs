{-# LANGUAGE NamedFieldPuns #-}

module BoatSim where

import Data.Complex (Complex((:+)), phase, cis)

import Graphics.Gloss.Interface.Pure.Simulate

import Boat
import Controller.PID


---------- MAIN LOOP STUFF ----------


data BoatSim = BoatSim { boat :: Boat
                       , xPID :: PID C
                       , thetaPID :: PID R
                       }


initial :: BoatSim
initial = BoatSim { boat = Boat { m = 0.1
                                , i = 1
                                , r = 1
                                , fMax = 30
                                , f1 = 0
                                , f2 = 0
                                , x = 0
                                , v = 0
                                , theta = 0
                                , omega = 0
                                }
                  , xPID = pidController (0.5, 0, 10) target noFilter noFilter 0
                  , thetaPID = pidController (5, 0, 8) 0 noFilter noFilter 0
                  }
  where
    target = (-100) :+ 100


validControl :: Boat -> (R, R) -> (R, R)
validControl Boat{r, fMax} (t, f) = (f1, f2)
  where
    t' = if t < 0 then -t else t
    f' = if f < fMax then 2*fMax - f else f

    validT' = fMax / (((f'-fMax)/t') + (1/r))
    validF' = -validT'/r + 2*fMax

    validT = if t < 0 then -validT' else validT'
    validF = if f < fMax then 2*fMax - validF' else validF'

    f1 = validT/(2*r*fMax) + validF/(2*fMax)
    f2 = -validT/(2*r*fMax) + validF/(2*fMax)


control :: Real t => t -> BoatSim -> BoatSim
control dt BoatSim{boat, xPID, thetaPID} = BoatSim boat' xPID' thetaPID'
  where
    dot :: C -> C -> R
    dot (r1 :+ i1) (r2 :+ i2) = r1*r2 + i1*i2

    (control, xPID') = getControl dt (x boat) xPID
    targetForce = control `dot` cis (theta boat)
    targetAngle = phase control
    (targetTorque, thetaPID') =  getControl dt (theta boat) thetaPID{pidTarget=targetAngle}

    (f1, f2) = validControl boat (targetTorque, targetForce)
    boat' = boat{f1=f1, f2=f2}


mainStep :: ViewPort -> Float -> BoatSim -> BoatSim
mainStep _ deltaFloat state = state'{boat=boat'}
  where
    dt = realToFrac deltaFloat
    state' = control dt state
    boat' = step dt (boat state')


renderSim :: BoatSim -> Picture
renderSim BoatSim{boat, xPID=PID{pidTarget}} = pictures [render boat, target]
  where
    target = move pidTarget $ color red $ circle 5
    move (x :+ y) = translate (realToFrac x) (realToFrac y)


boatMain :: IO ()
boatMain = simulate display background fps initial renderSim mainStep
  where
    display = InWindow "Control Systems" (1200, 800) (100, 100)
    background = black
    fps = 60