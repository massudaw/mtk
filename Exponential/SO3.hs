{-# LANGUAGE ExistentialQuantification , DeriveFunctor,FlexibleContexts,ImpredicativeTypes,TypeFamilies #-}

module Exponential.SO3 where

import Exponential.Class

import Control.Lens

import Linear.Vector
import Linear.V3
import Linear.Affine
import Linear.Matrix
import Linear.Metric
import Local

import Rotation.SO3
import Space.Class
import qualified Debug.Trace as T


instance Exponential SO3 where
    logM = logM3 . unSO3
    expM = SO3 . expM3

data CSO3 a = CSO3 { unCSO3 :: V3 (V3 a)} deriving(Show,Read,Functor)

expM3 v@(V3 x y z)
    = V3
        (V3 (cos t + c*x^2) (-s*z +c*x*y) (s*y+c*x*z))
        (V3 (s*z+c*x*y) (cos t + c*y^2) (-s*x+c*y*z))
        (V3 (-s*y+c*x*z ) (s*x+c*y*z) (cos t + c*z^2))
    where
        t = norm v
        c =  (1 - cos t)/(t^2 + 1.24e-16)
        s = sinc t

logM3 :: (Floating a, Ord a) => (V3 (V3 a)) -> V3 a
logM3 v = (t /( 2*sin t + 1.24e-16)) *^ (V3 (v^._z._y - v^._y._z) (v^._x._z - v^._z._x) (v^._y._x - v^._x._y))
    where
        t = acosc $ (trace v - 1)/2

acosc x
 | x > 1 = acos 1
 | x < -1 = acos(-1)
 | otherwise = acos x

sinc x
 | x == 0 = 1
 | otherwise = sin x / x


