{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WingModel
    ( wingModel
    ) where

import Types ( Alpha, B, ForceMoment(..), Wing(..) )

-- | Models the Wing force moment given the commanded thrust
-- L = q S Cl
-- D = q S Cd
-- M = q S C Cm
-- q = dynamic pressure = 1/2 rho V^2
wingModel :: forall a . Floating a => Alpha a -> Wing a ->  ForceMoment B a
wingModel _ _ = wingForceMoment
  where
    -- q
    wingForceMoment = pure 0

