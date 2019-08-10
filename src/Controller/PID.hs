{-# LANGUAGE NamedFieldPuns #-}

module Controller.PID where


data PID a = PID { pidConstants :: (a, a, a)
                 , pidTarget :: a
                 , pidIntTransform :: (a, a) -> a
                 , pidDerTransform :: (a, a) -> a
                 , pidErrInt :: a
                 , pidPrevPos :: a
                 }


lowPassFilter :: Num a => a -> (a, a) -> a
lowPassFilter coef (prev, current) = coef * prev + (1-coef) * current


noFilter :: (a, a) -> a
noFilter = snd


predicateFilter :: Num a => (a -> Bool) -> (a, a) -> a
predicateFilter predicate (_, v)
  | predicate v = v
  | otherwise = 0


pidController :: Fractional a => (a, a, a) -> a -> ((a, a) -> a) -> ((a, a) -> a) -> a -> PID a
pidController constants target intTransform derTransform initialPos = PID constants target intTransform derTransform 0 initialPos


getControl :: (Fractional a, Real t) => t -> a -> PID a -> (a, PID a)
getControl dt pos pid@PID{pidConstants=(kp, ki, kd), pidTarget, pidIntTransform, pidErrInt, pidPrevPos}
  = (control, pid{pidErrInt=errInt', pidPrevPos=pos})
  where
    err = pidTarget - pos
    errInt' = pidErrInt + pidIntTransform (pidErrInt, realToFrac dt * err)
    errDeriv = (pidPrevPos - pos) / realToFrac dt
    control = kp*err + ki*errInt' + kd*errDeriv
