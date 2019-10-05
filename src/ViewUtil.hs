module ViewUtil where

import Graphics.Gloss.Interface.Pure.Simulate

arrow :: Real a => (a, a) -> (a, a) -> (Color, Color) -> a -> Picture
arrow wBound lBound (low, high) x = color c $ pictures [cap, point]
  where
    interpolate (a, b) = b*x + a*(1-x)
    w = realToFrac $ interpolate wBound
    l = realToFrac $ interpolate lBound
    point = polygon [(0, w/2), (0, (-w)/2), (l, 0)]
    cap = circleSolid (w/2)
    c = mixColors (realToFrac (1-x)) (realToFrac x) low high


-- | rotate by an angle in radians counter clockwise
rotateRad :: Float -> Picture -> Picture
rotateRad = rotate . negate . (*(180/pi))
