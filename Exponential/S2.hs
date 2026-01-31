{-# LANGUAGE TypeFamilies #-}

module Exponential.S2 where

import Exponential.Class
import Rotation.S2
import Linear.V3
import Linear.Metric
import Linear.Vector
import Control.Lens

instance Exponential S2 where
    logM = logS2 . unS2
    expM = S2 . expS2

-- Exponential map: maps tangent vector at north pole to point on sphere
-- exp_p(v) = cos(θ)*p + sin(θ)*(v/||v||) where θ = ||v||
-- For north pole p = (0,0,1), this simplifies
expS2 :: (Floating a, Ord a) => V3 a -> V3 a
expS2 v
    | theta < epsilon = V3 0 0 1  -- v ≈ 0, return north pole
    | otherwise = (cos theta *^ p) ^+^ (sinc_theta *^ v)
    where
        p = V3 0 0 1  -- north pole as base point
        theta = norm v
        sinc_theta = sinc theta
        epsilon = 1e-10

-- Logarithmic map: maps point on sphere to tangent vector at north pole
-- log_p(q) = θ * (q - cos(θ)*p) / sin(θ) where θ = arccos(p·q)
logS2 :: (Floating a, Ord a) => V3 a -> V3 a
logS2 q
    | abs cos_theta >= 1 - epsilon = V3 0 0 0  -- q ≈ p, return zero
    | cos_theta < -1 + epsilon = error "S2.logS2: antipodal points (logarithm not unique)"
    | otherwise = (theta / sin_theta) *^ (q ^-^ (cos_theta *^ p))
    where
        p = V3 0 0 1  -- north pole as base point
        cos_theta = clamp (-1) 1 (p `dot` q)
        theta = acos cos_theta
        sin_theta = sin theta
        epsilon = 1e-10

-- sinc function: sin(x)/x with proper limit at 0
sinc :: (Floating a, Ord a) => a -> a
sinc x
    | abs x < 1e-10 = 1 - x^2/6  -- Taylor series approximation
    | otherwise = sin x / x

-- Clamp a value between min and max
clamp :: Ord a => a -> a -> a -> a
clamp mn mx = max mn . min mx
