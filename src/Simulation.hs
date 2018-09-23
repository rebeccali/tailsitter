{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      :  Simulation
Copyright   :  (c) Rebecca Li 2018
License     :  MIT
Maintainer  :  Rebecca Li <lunojk@gmail.com>
Stability   :  Work In Progress
Portability :  GHC
Simulation for 2D Tailsitter
-}

module Simulation
    ( planeModel
    ) where


import Linear ( (^+^) )
import SpatialMathT ( rot', Quaternion(..), Rot(..), V3(..), V3T(..) )

import PropellorModel
import Types
import WingModel


planeModel :: forall a . Floating a
           => Timestep a
           -> TailsitterParams a
           -> TailsitterState a
           -> TailsitterState a
planeModel dt params oldState = newState
  where
    newState =
      TailsitterState
      { sr_n2b_n = pure 0
      , sQuatN2B = pure 0
      , sv_n2b_n = pure 0
      , sw_n2b_n = pure 0
      , sddtw_n2b_n = ddtw_n2b_n
      , sddtv_n2b_n = pure 0
      }

    ddtw_n2b_n = V3T $ (/) <$> pInertia params <*> unV totalMoment

    alpha :: Alpha a
    alpha = Alpha 0

    wingFm_b :: ForceMoment B a
    wingFm_b = wingModel alpha (pWing params)

    propFm_b :: ForceMoment B a
    propFm_b = propellorModel (Thrust 0) (pPropellor params)

    forceMoment_b2n :: ForceMoment B a -> ForceMoment N a
    forceMoment_b2n (ForceMoment f m) = ForceMoment (rotation f) (rotation m)
      where
        rotation = rot' $ sQuatN2B oldState

    gravity_n :: ForceMoment N a
    gravity_n =
      ForceMoment
      { fmForces = V3T (V3 0 (-9.81 * pMass params) 0)
      , fmMoments = pure 0
      }

    totalForce :: V3T N a
    totalMoment :: V3T N a

    ForceMoment totalForce totalMoment =
      (forceMoment_b2n propFm_b) ^+^
      (forceMoment_b2n wingFm_b) ^+^
      gravity_n
