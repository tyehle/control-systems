{-# LANGUAGE NamedFieldPuns #-}

module ZambiniSim where

import Data.Complex (Complex((:+)), phase, cis)

import Graphics.Gloss.Interface.Pure.Simulate

import Zambini
import Controller.PID


---------- MAIN LOOP STUFF ----------


data ZambiniSim = ZambiniSim { zambini :: Zambini
                             , xPID :: PID C
                             , thetaPID :: PID R
                             }


initial :: ZambiniSim
initial = ZambiniSim { zambini = Zambini { m = 0.1
                                         , i = 1
                                         , r = 1
                                         , fMax = 300
                                         , f1 = 0
                                         , f2 = 0
                                         , x = 0
                                         , v = 0
                                         , theta = 0
                                         , omega = 0
                                         }
                     , xPID = pidController (3, 0.01, 4) target noFilter noFilter 0
                     , thetaPID = pidController (5, 0, 8) 0 noFilter noFilter 0
                     }
  where
    target = (-100) :+ 100


validControl :: Zambini -> (R, R) -> (R, R)
validControl Zambini{r, fMax} (t, f) = (f1, f2)
  where
    t' = if t < 0 then -t else t
    f' = if f < fMax then 2*fMax - f else f

    validT' = fMax / (((f'-fMax)/t') + (1/r))
    validF' = -validT'/r + 2*fMax

    validT = if t < 0 then -validT' else validT'
    validF = if f < fMax then 2*fMax - validF' else validF'

    f1 = validT/(2*r*fMax) + validF/(2*fMax)
    f2 = -validT/(2*r*fMax) + validF/(2*fMax)


control :: Real t => t -> ZambiniSim -> ZambiniSim
control dt ZambiniSim{zambini, xPID, thetaPID} = ZambiniSim zambini' xPID' thetaPID'
  where
    dot :: C -> C -> R
    dot (r1 :+ i1) (r2 :+ i2) = r1*r2 + i1*i2

    (control, xPID') = getControl dt (x zambini) xPID
    targetForce = control `dot` cis (theta zambini)
    targetAngle = phase control
    (targetTorque, thetaPID') =  getControl dt (theta zambini) thetaPID{pidTarget=targetAngle}

    (f1, f2) = validControl zambini (targetTorque, targetForce)
    zambini' = zambini{f1=f1, f2=f2}


mainStep :: ViewPort -> Float -> ZambiniSim -> ZambiniSim
mainStep _ deltaFloat state = state'{zambini=zambini'}
  where
    dt = realToFrac deltaFloat
    state' = control dt state
    zambini' = step dt (zambini state')


renderSim :: ZambiniSim -> Picture
renderSim ZambiniSim{zambini, xPID=PID{pidTarget}} = pictures [render zambini, target]
  where
    target = move pidTarget $ color red $ circle 5
    move (x :+ y) = translate (realToFrac x) (realToFrac y)


zambiniMain :: IO ()
zambiniMain = simulate display background fps initial renderSim mainStep
  where
    display = InWindow "Control Systems" (1200, 800) (100, 100)
    background = black
    fps = 60