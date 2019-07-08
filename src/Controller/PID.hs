{-# LANGUAGE NamedFieldPuns #-}

module Controller.PID where


pControl :: Num a => (a, b, b) -> a -> a
pControl (kp, _, _) err = kp * err


piControl :: Num a => (a, a, b) -> a -> a
piControl (kp, ki, _) err = kp * err + ki * 0


data PID a = PID { pidConstants :: (a, a, a)
                 , pidTarget :: a
                 , pidIntFilter :: a -> Bool
                 , pidErrInt :: a
                 , pidPrevPos :: a
                 }


pidController :: Fractional a => (a, a, a) -> a -> (a -> Bool) -> a -> PID a
pidController constants target intFilter initialPos = PID constants target intFilter 0 initialPos
  where
    err = target - initialPos


getControl :: (Fractional a, Real t) => t -> a -> PID a -> (a, PID a)
getControl dt pos pid@PID{pidConstants=(kp, ki, kd), pidTarget, pidIntFilter, pidErrInt, pidPrevPos}
  = (control, pid{pidErrInt=errInt', pidPrevPos=pos})
  where
    err = pidTarget - pos
    errInt' = pidErrInt + if pidIntFilter err then realToFrac dt * err else 0
    errDeriv = (pidPrevPos - pos) / realToFrac dt
    control = kp*err + ki*errInt' + kd*errDeriv
