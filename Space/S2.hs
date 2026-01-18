{-# LANGUAGE StandaloneDeriving,FlexibleContexts,MultiParamTypeClasses,TypeOperators,FlexibleInstances,DeriveFunctor,DeriveFoldable,DeriveTraversable,GeneralizedNewtypeDeriving,DeriveDataTypeable,TypeFamilies #-}

module Space.S2 where

import Control.Applicative
import Space.Class
import Linear.V3
import Linear.Metric
import Linear.Vector
import Data.Data
import Control.Lens

import Rotation.S2

-- Space instance for S2 (2-sphere)
-- The manifold dimension is 2 (it's a 2D surface)
-- The tangent space is represented as V3 (ambient 3D space) but vectors must be orthogonal to the base point
instance Space S2 where
    dim _ _ = 2

    -- Exponential map: exp_p(v) moves from point p along geodesic in direction v
    -- Formula: exp_p(v) = cos(θ)*p + sinc(θ)*v where θ = ||v||
    p |+| v
        | theta < epsilon = p  -- v ≈ 0, stay at p
        | otherwise = S2 $ (cos theta *^ unS2 p) ^+^ (sinc_theta *^ v)
        where
            theta = norm v
            sinc_theta = sinc theta
            epsilon = 1e-10

    -- Logarithmic map: log_p(q) finds tangent vector at p pointing toward q
    -- Formula: log_p(q) = (θ / sin θ) * (q - cos(θ)*p) where θ = arccos(p·q)
    q |-| p
        | abs cos_theta >= 1 - epsilon = zero  -- q ≈ p, return zero vector
        | otherwise = (theta / sin_theta) *^ (unS2 q ^-^ (cos_theta *^ unS2 p))
        where
            cos_theta = clamp (-1) 1 ((unS2 p) `dot` (unS2 q))
            theta = acos cos_theta
            sin_theta = max epsilon (sin theta)  -- avoid division by zero
            epsilon = 1e-10

-- sinc function: sin(x)/x with proper limit at 0
sinc :: (Floating a, Ord a) => a -> a
sinc x
    | abs x < 1e-8 = 1 - x^2/6  -- Taylor series approximation
    | otherwise = sin x / x

-- Clamp a value between min and max
clamp :: Ord a => a -> a -> a -> a
clamp mn mx = max mn . min mx
