{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PropellorModel
    ( propellorModel
    ) where

import Types ( B, ForceMoment(..), Propellor(..), Thrust)

-- | Models the propellor force moment given the commanded thrust
propellorModel :: forall a . Floating a => Thrust a -> Propellor a ->  ForceMoment B a
propellorModel _ _ = propellorForceMoment
  where
    propellorForceMoment = pure 0

