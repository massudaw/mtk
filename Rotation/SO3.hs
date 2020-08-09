{-# LANGUAGE
 StandaloneDeriving,
 FlexibleContexts,
 MultiParamTypeClasses,
 TypeOperators,
 FlexibleInstances,
 DeriveFunctor,
 GeneralizedNewtypeDeriving,
 DerivingVia,
 DerivingStrategies,
 DeriveDataTypeable,
 TypeFamilies
 #-}

module Rotation.SO3 where


import Data.Data
import Linear.V3 hiding (_yz,_xy,_zx)
import Linear.V2
import Linear.V1
import Linear.Matrix
--import MultiLinear.Class

import Control.Lens
import Data.Functor.Classes
import Data.Functor.Compose
import Control.Applicative
import Local
import As
import Rotation.SO2

import Data.Distributive
type instance Local SO3 = V3

type M3 a = V3 (V3 a)

--instance (RealFloat a,Show a)=> Show (SO3 a) where
    --show x = "SO3" ++ show (so3ToPhi x , so3ToTheta x, so3ToPsi x)

newtype SO3 a = SO3 {unSO3 :: M3 a }deriving(Functor,Data,Typeable,Eq,Read,Show)
                                    deriving Show1 via (Compose V3 V3 )


angles m = V3 ( so3ToPhi m) (so3ToTheta m) (so3ToPsi m)
unRot213 (SO3 m ) = V3 phi theta psi
    where
        phi = atan2 (- m ^. _x._z) (m ^. _z._z)
        theta = asin (m ^. _y._z)
        psi = atan2 (- m ^. _y._x) (m ^. _y._y)


unRot132 (SO3 m ) = V3 phi theta psi
    where
        phi = atan2 (- m ^. _z._y) (m ^. _y._y)
        theta = asin (m ^. _x._y)
        psi = atan2 (- m ^. _x._z) (m ^. _x._x)

unRot321 (SO3 m ) = V3 phi theta psi
    where
        phi = atan2 (m ^. _y._x) (m ^. _x._x)
        theta = asin (m^. _z._x)
        psi = atan2 ( -m ^. _z._y) (m ^. _z._z)

unRot123 x = V3 (so3ToPhi x) (so3ToTheta x) (so3ToPsi x)

so3ToPhi (SO3 m)= atan2 (m ^._y._z) ( m ^._z._z)
so3ToTheta (SO3 m)= -asin (m^._x._z)
so3ToPsi (SO3 m)= atan2 (m ^._x._y) ( m ^._x._x)

transposition angle =[distribute (rotM angle), rotM (fmap negate angle) ]

testRot angle =(r2  !*! r2 !*! r )!* (V3 1 1 1) where
    r = rotM angle
    r2 = distribute $ rotM (fmap (/2) angle)

rotM (V3 ax ay az ) =  rotX (V1 ax )!*! rotY (V1 ay) !*! rotZ (V1 az)
rotation = SO3 . rotM

_yz f (V3 a b c) = (\(V2 b' c') -> V3 a b' c') <$> f (V2 b c)
_zx f (V3 a b c) = (\(V2 c' a') -> V3 a' b c') <$> f (V2 c a)


identV3 :: Num a => M3 a
identV3
    = V3
        (V3 1 0 0 )
        (V3 0  1 0 )
        (V3 0 0  1 )

skewV3 gyro = alongMatrix _xy .~(skewM2 z)$ alongMatrix _yz .~(skewM2 x)$ alongMatrix _zx .~(skewM2 y)$ 0
    where
      x = V1 $ gyro ^. _x
      y = V1 $ gyro ^. _y
      z = V1 $ gyro ^. _z

rotX  theta = alongMatrix _yz .~ (rotationSO2 theta) $ identV3
rotY  theta = alongMatrix _zx .~ (rotationSO2 theta ) $ identV3
rotZ  theta = alongMatrix _xy .~ (rotationSO2 theta ) $ identV3
