{-# LANGUAGE FlexibleContexts,MultiParamTypeClasses,TypeOperators,FlexibleInstances,DeriveFunctor,DeriveFoldable,DeriveTraversable,GeneralizedNewtypeDeriving,DeriveDataTypeable,TypeFamilies #-}

module Space.SE3 where

import Control.Applicative
import Space.Class
import Rotation.SO3
import Exponential.SO3
import Exponential.SO2
import Exponential.Class
import Rotation.SO2
import Space.SO3
import Space.SO2

import CartesianProduct
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.Vector
import Linear.Metric
import Linear.Matrix
import Data.Data
import Control.Lens hiding((|>))
import Data.Distributive
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)
import Exponential.Class

import SemiProduct
import Ellipsoid

import Local



so3ToPhi (SO3 m)= atan2 (m ^._z._y) ( m ^._z._z)
so3ToPsi (SO3 m)= atan2 (m ^._y._x) ( m ^._x._x)
so3ToTheta (SO3 m)= - atan2 (m ^. _z ._x) (sqrt $ 1 - (m ^. _z ._x)^2 )

type NED = SO3 :>: EllipsoidSurface
type SE3 =  SO3 :>: V3

identV2
    = V2
        (V2 1 0)
        (V2 0 1)

skewV2 (V1 xy)
    = V2
        (V2 0 xy)
        (V2 (-xy) 0)

instance Space NED where
   (x :>: y) |+| i =  (x `mult` dx ):>: ( y |+| dy)
	where (dx :>: dy) =  expSE3 i
   (x :>: y) |-| (i :>: j)  = logSE3 ((invert i `mult` x) :>:  (j |-|y )  )

instance Space SE3 where
   x |+| y = x `mult` expM y
   x |-| y = logM (invert y `mult` x) 


instance Exponential SE3 where
   logM = logSE3
   expM = expSE3

type SE2 = SO2 :>: V2

--expSE2 :: Floating a => (V1 :|: V2 ) a -> SE2 a
expSE2 (a@(V1 t) :|: (V2 x y)) = (SO2 $ expM2 a) :>: (V2 (x* sin t /t -y*(1- cos t)/t) (x*(1-cos t)/t + y*sin t/t))


--logSE2 :: Floating a => SE2 a -> (V1 :|: V2 ) a  
logSE2 ((SO2 r) :>: (V2 x y)) = w :|:  (V2((x*t*sin t)/(2*(1 - cos t)) + y*t/2) (y*t*sin t/(2*(1 - cos t)) - x*t/2))
    where w = logM2 r
          t = norm w

expR3 :: Floating a => V3 a  -> M3 a 
expR3 w = identV3
        + ((1- cos t)/(t + 1e-20))
        *^^ skewV3 w
        + ((1 - cos t)/(t^2 +1e-20))
        *^^ (skewV3 w !*! skewV3 w)
    where t = norm w

--logR3 :: Floating a => M3 a  -> M3 a 
logR3 wM = identV3
        - 0.1 *^^ sk
        + ((2*sin (norm w) - norm w * (1 + cos(norm w))) / (2*(norm w)^2*sin (norm w) + 1e-19))
        *^^ (sk !*! sk)
    where w = logM3 wM
          sk = skewV3 w


--expSE3 :: (Eq a ,Floating a ) => (V3 :|: V3 ) a  ->  SE3 a
expSE3 (w :|: t)=  (SO3 $ expM3 w) :>: ( t *! (expR3 w)) 

--logSE3 ::  Floating a => SE3 a -> (V3 :|: V3 ) a 
logSE3 (SO3 w :>: t )= (logM3 w) :|: (logR3 w !* t)

