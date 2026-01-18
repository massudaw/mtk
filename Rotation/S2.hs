{-# LANGUAGE
 StandaloneDeriving,
 FlexibleContexts,
 MultiParamTypeClasses,
 TypeOperators,
 FlexibleInstances,
 DeriveFunctor,
 GeneralizedNewtypeDeriving,
 DeriveDataTypeable,
 TypeFamilies
 #-}

module Rotation.S2 where

import Data.Data
import Control.Lens
import Linear.V3
import Linear.V2
import Linear.Metric
import Local

-- S2 represents points on the unit sphere in R^3
-- Invariant: norm (unS2 s) == 1
newtype S2 a = S2 {unS2 :: V3 a} deriving(Functor, Data, Typeable, Eq, Show, Read)

-- The tangent space at any point on S2 is 3-dimensional in the ambient space
-- but constrained to be orthogonal to the base point
type instance Local S2 = V3

-- Create an S2 from a V3 by normalizing it
mkS2 :: (Floating a, Eq a) => V3 a -> S2 a
mkS2 v = S2 $ normalize v

-- North pole of the sphere
northPole :: Num a => S2 a
northPole = S2 (V3 0 0 1)

-- South pole of the sphere
southPole :: Num a => S2 a
southPole = S2 (V3 0 0 (-1))
