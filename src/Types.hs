{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}


{- |
Module      :  Types
Copyright   :  (c) Rebecca Li 2018
License     :  MIT
Maintainer  :  Rebecca Li <lunojk@gmail.com>
Stability   :  Work In Progress
Portability :  GHC
Types for 2D Tailsitter
= Documentation
Notes on notation:
r_b2propellor_b : "r_" denotes the vector from "b" the body frame center "2" to "propellor"
                  the propellor center "_b" in the body frame
u_thrust_b : "u_" denotes the unit vector of the direction of the thrust in the body frame.
-}

module Types
    ( B, N, W
    , Alpha(..)
    , ForceMoment(..)
    , Propellor(..)
    , TailsitterParams(..)
    , TailsitterState(..)
    , Timestep(..)
    , Thrust(..)
    , Wing(..)
    ) where

import Control.Applicative ( Applicative, pure)
import Data.Foldable ( Foldable )
import Data.Traversable ( Traversable )
import GHC.Generics ( Generic, Generic1 )

import Linear ( Additive(..) )
import SpatialMathT ( Rot(..), Quaternion(..), V3T(..), V3(..) )

-- Frame types

-- | Wind Frame
data W

-- | Body Frame
data B

-- | NED Frame
data N

-- Units

-- | Thrust
newtype Thrust a = Thrust a deriving (Eq, Show, Ord, Generic1, Generic)

-- | Alpha as in angle of attack
newtype Alpha a = Alpha a deriving (Eq, Show, Ord, Generic1, Generic)

-- | Timestep is the length of time
newtype Timestep a = Timestep a deriving (Eq, Show, Ord, Generic1, Generic)


-- | Describes the forces and moments. The phantom type is the frame.
data ForceMoment f a
 = ForceMoment
 { fmForces :: V3T f a
 , fmMoments :: V3T f a
 } deriving (Functor, Foldable, Traversable, Eq, Show, Ord, Generic1, Generic)

instance Applicative (ForceMoment f) where
  pure x = ForceMoment (V3T (V3 x x x)) (V3T (V3 x x x))
  (ForceMoment a b) <*> (ForceMoment c d) = ForceMoment (a <*> c) (b <*> d)

instance Additive (ForceMoment f) where
  zero = pure 0


data Propellor a
 = Propellor
 { pr_b2propellor_b :: V3T B a -- ^ body center to propellor rotor center
 , pu_thrust_b :: V3T B a -- ^ Unit thrust vector in body frame
 , pThrustCoeff :: a -- ^ Thrust coefficient for input between 0 and 1
 }

data Wing a
  = Wing
  { pr_b2wing_b :: V3T B a -- ^ body center to wing leading edge
  , pS :: a -- ^ Reference surface area for lift calculations
  , pC :: a -- ^ Chord length
  }

data TailsitterParams a
  = TailsitterParams
  { pWing :: Wing a
  , pPropellor :: Propellor a
  , pMass :: a -- ^ mass in kilograms
  , pInertia :: V3 a -- ^ inertia diagonal
  }

data TailsitterState a
  = TailsitterState
  { sr_n2b_n :: V3T N a -- ^ current position in NED
  , sQuatN2B :: Rot N B Quaternion a -- ^ rotation from NED to body frame
  , sv_n2b_n :: V3T N a -- ^ current velocity in NED
  , sw_n2b_n :: V3T N a -- ^ current angular velocity in NED
  , sddtw_n2b_n :: V3T N a -- ^ current angular acceleraiton in NED
  , sddtv_n2b_n :: V3T N a -- ^ current acceleration in NED
  }

