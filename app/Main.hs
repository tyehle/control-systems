module Main where

import Data.Complex
import Graphics.Gloss.Interface.Pure.Simulate

import Controller.PID
import ViewUtil
import BoatSim

type C = Complex Double

data Ship = Ship { shipMass :: Double
                 , shipPos :: C
                 , shipVel :: C
                 , shipController :: PID C
                 , shipControlMax :: Double
                 , shipForce :: C
                 , shipControl :: C
                 }

data Model = Model { modelShip :: Ship
                   , modelAttractors :: [(Double, C)]
                   }


-------------------- Graphics ---------------


controlGraphic :: Real a => a -> Picture
controlGraphic = translate 20 0 . arrow (5, 20) (15, 100) (low, high)
  where
    low  = makeColorI 0 64 192 255
    high = makeColorI 0 160 192 255


forceGraphic :: Real a => a -> Picture
forceGraphic = arrow (5, 20) (15, 100) (low, high)
  where
    low  = makeColorI 192 0   64  255
    high = makeColorI 192 160 0   255


crosshair :: Picture
crosshair = pictures [vert, rotate 90 vert]
  where
    vert = line [(0, 5), (0, -5)]


shipGraphic :: Picture
shipGraphic = polygon [(-5,0), (-10, 15), (25, 0), (-10, -15)]


render :: Model -> Picture
render Model{modelShip=ship, modelAttractors=attractors}
  = pictures [forceArrow, controlArrow, shipPic, attractorPics, target, errPic]
  where
    move (x :+ y) = translate (realToFrac x) (realToFrac y)
    toShip = move $ shipPos ship
    shipPic = color (greyN 0.5) $ toShip $ rotateRad (realToFrac $ phase (shipControl ship)) shipGraphic
    attractorPics = pictures $ map (\(_,pos) -> move pos $ color blue crosshair) attractors
    target = move (pidTarget $ shipController ship) $ color (dark . dark $ green) $ circle 5
    err = pidTarget (shipController ship) - shipPos ship
    errPic = translate (-400) 300 $ color white $ scale 0.25 0.25 $ pictures
      [ text "Error"
      , translate 0 (-150) $ text $ show $ realPart err
      , translate 0 (-300) $ text $ show $ imagPart err
      ]
    arrowScale = 1
    forceArrow   = toShip $ rotateRad (realToFrac $ phase (shipForce ship) + pi) $ forceGraphic (min 1 $ magnitude (shipForce ship) * arrowScale)
    controlArrow = toShip $ rotateRad (realToFrac $ phase (shipControl ship) + pi) $ controlGraphic (min 1 $ magnitude (shipControl ship) * arrowScale)


-------------------- Initial ---------------


initial :: Model
initial = Model { modelShip = Ship { shipMass = 1
                                   , shipPos = initialPos
                                   , shipVel = 4 :+ (-5)
                                   , shipController = pidController
                                                        (0.01, 0.0002, 0.15)
                                                        target
                                                        (predicateFilter $ (< 100) . magnitude)
                                                        noFilter
                                                        initialPos
                                   , shipControlMax = 1
                                   , shipForce = 0 :+ 0
                                   , shipControl = 0 :+ 0
                                   }
                , modelAttractors = [(15000, 0 :+ 200), (15000, 0 :+ (-200)), (15000, (-300) :+ 150)]
                }
  where
    initialPos = 100 :+ 100
    target = (-300) :+ (-100)


-------------------- Updates ---------------


attractionForce :: Ship -> (Double, C) -> C
attractionForce ship (mass, pos) = direction * realToFrac scalar
  where
    direction = pos - shipPos ship
    scalar = mass * shipMass ship / ( magnitude direction ^ (3::Integer) )


step :: ViewPort -> Float -> Model -> Model
step _ deltaFloat rawModel =
  m{modelShip=ship{shipPos=pos', shipVel=vel', shipForce=ef, shipControl=cf}}
  where
    delta = realToFrac deltaFloat * 10
    m@Model{modelShip=ship} = environmentForces rawModel{modelShip = controlForces delta (modelShip rawModel)}
    ef = shipForce ship
    cf = shipControl ship
    force = ef + cf
    accel = force / realToFrac (shipMass ship)
    vel' = shipVel ship + accel * realToFrac delta
    pos' = shipPos ship + vel' * realToFrac delta


environmentForces :: Model -> Model
environmentForces model@Model{modelShip=ship, modelAttractors=attractors}
  = model{modelShip=ship{shipForce=force}}
  where
    force = sum $ map (attractionForce ship) attractors


limitMagnitude :: Double -> C -> C
limitMagnitude peak c
  | magnitude c > peak = c / realToFrac (magnitude c / peak)
  | otherwise = c


controlForces :: Double -> Ship -> Ship
controlForces dt ship = ship{shipControl=limited, shipController=pid'}
  where
    (control, pid') = getControl dt (shipPos ship) (shipController ship)
    limited = limitMagnitude (shipControlMax ship) control


-------------------- Main ---------------


main :: IO ()
main = boatMain
-- main = simulate display background fps initial render step
--   where
--     display = InWindow "Control Systems" (1200, 800) (100, 100)
--     background = black
--     fps = 60
