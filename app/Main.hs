module Main where

import Data.Complex
import Graphics.Gloss.Interface.Pure.Simulate

import Controller.PID

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
                   , modelAttractor :: (Double, C)
                   }


-------------------- Graphics ---------------


arrow :: Real a => (a, a) -> (a, a) -> (Color, Color) -> a -> Picture
arrow wBound lBound (low, high) x = color c shape
  where
    interpolate (a, b) = b*x + a*(1-x)
    w = realToFrac $ interpolate wBound
    l = realToFrac $ interpolate lBound
    shape = polygon [(0, w/2), (0, (-w)/2), (l, 0)]
    c = mixColors (realToFrac (1-x)) (realToFrac x) low high


controlGraphic :: Real a => a -> Picture
controlGraphic = arrow (5, 20) (15, 100) (low, high)
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


render :: Model -> Picture
render Model{modelShip=ship, modelAttractor=(_,aPos)}
  = pictures [forceArrow, controlArrow, ball, attractor, target, errPic]
  where
    move (x :+ y) = translate (realToFrac x) (realToFrac y)
    toShip = move $ shipPos ship
    ball = color (greyN 0.5) $ toShip $ thickCircle 0 30
    attractor = move aPos $ color blue crosshair
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

-- | rotate by an angle in radians counter clockwise
rotateRad :: Float -> Picture -> Picture
rotateRad = rotate . negate . (*(180/pi))


-------------------- Initial ---------------


initial :: Model
initial = Model { modelShip = Ship { shipMass = 1
                                   , shipPos = initialPos
                                   , shipVel = 4 :+ (-5)
                                   , shipController = pidController (0.01, 0.0002, 0.15) target (\e -> magnitude e < 100) initialPos
                                   , shipControlMax = 1
                                   , shipForce = 0 :+ 0
                                   , shipControl = 0 :+ 0
                                   }
                , modelAttractor = (15000, 0 :+ 0)
                }
  where
    initialPos = 100 :+ 100
    target = (-300) :+ (-100)


-------------------- Updates ---------------


attractionForce :: Double -> C -> Ship -> C
attractionForce mass pos ship = direction * realToFrac scalar
  where
    direction = pos - shipPos ship
    scalar = mass * shipMass ship / ( magnitude direction ^ 3 )


step :: ViewPort -> Float -> Model -> Model
step view deltaFloat rawModel =
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
environmentForces model@Model{modelShip=ship, modelAttractor=(m,x)}
  = model{modelShip=ship{shipForce=force}}
  where
    force = attractionForce m x ship


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
main = simulate display background fps initial render step
  where
    display = InWindow "Control Systems" (1200, 800) (100, 100)
    background = black
    fps = 60
