{-# LANGUAGE ExistentialQuantification , DeriveFunctor,FlexibleContexts,ImpredicativeTypes,TypeFamilies #-}

module Exponential.SO3 where

import Exponential.Class
import Data.Complex

import Control.Lens

import Linear.Vector
import Linear.V3
import Linear.Affine
import Linear.Matrix
import Linear.Metric
import Local
import Linear.V4

import Rotation.SO3
import Space.Class
import qualified Debug.Trace as T


instance Exponential SO3 where
    logM = logM3 . unSO3
    expM = SO3 . expM3

data CSO3 a = CSO3 { unCSO3 :: V3 (V3 a)} deriving(Show,Read,Functor)

mkLorentz :: Num a => M33 a -> V3 a -> M44 a
mkLorentz (V3 r1 r2 r3) (V3 tx ty tz) =
  V4 (snoc3 r1 tx) (snoc3 r2 ty) (snoc3 r3 tz) (V4 tx ty tz 0)
  where snoc3 (V3 x y z) = V4 x y z


expM31 v1@(V3 ox oy oz) v2@(V3 dx dy dz)  = foldr1 (!+!) $ zipWith (*!!) (fmap realPart [l1,l2,l3,l4]) [eye4,a , a!*!a , a!*!a !*!a]
    where 
        alpha = v1 `dot` v1  - v2 `dot` v2
        beta = v1 `dot` v2
        delta = sqrt $  alpha^2 + 4*beta
        lambda1 = 0 :+ sqrt (abs((alpha - delta )/2))
        lambda2 =  sqrt ((alpha + delta )/2) :+  0
        l1 = (-cosh(lambda1)*lambda2^2 + lambda1^2*cosh(lambda2))/(lambda1^2 - lambda2^2)
        l2 = (-sinh(lambda1)*lambda2^3 + lambda1^3*sinh(lambda2))/(lambda1^2 - lambda2^2)/lambda1/lambda2
        l3 = (cosh lambda1  + cosh lambda2 )/(lambda1^2 - lambda2^2)
        l4 = (lambda2^2 * sinh lambda1 - lambda1* sinh lambda2)/(lambda1^2 - lambda2^2)/lambda1/lambda2
        a = mkLorentz (skewV3 v1) v2
          
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


