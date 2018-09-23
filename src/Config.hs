{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}

module Types
    ( B, N, W
    , ForceMoment(..)
    , Thrust
    ) where

import Data.Foldable ( Foldable )
import Data.Traversable ( Traversable )
import GHC.Generics ( Generic, Generic1 )

import SpatialMathT ( V3T(..) )

-- Frame types

-- | Wind Frame
data W

-- | Body Frame
data B

-- | NED Frame
data N

-- Units

-- | Thrust
newtype Thrust a = Thrust a deriving (Functor, Foldable, Traversable, Eq, Show, Ord, Generic1, Generic)

-- | Describes the forces and moments. The phantom type is the frame.
data ForceMoment f a
 = ForceMoment
 { fmForces :: V3T f a
 , fmMoments :: V3T f a
 } deriving (Functor, Foldable, Traversable, Eq, Show, Ord, Generic1, Generic)
