{-# LANGUAGE TemplateHaskell,DeriveFunctor,DeriveFoldable,DeriveTraversable,DeriveDataTypeable,TypeFamilies #-}
module Ellipsoid where
import Linear.V3
import Data.Functor.Compose
import Control.Applicative
import Data.Foldable hiding(product)
import Space.SO3
import Rotation.SO3
import Exponential.SO3
import Control.Lens.TH
import Space.Class
import Data.Data
import Local


data EllipsoidSurface a =
    EllipsoidSurface
        {  _latitude :: a
        ,  _longitude  :: a
        ,  _heigth :: a
        ,  _radius :: a
        ,  _flatness :: a
        }deriving(Read,Show,Eq,Functor,Foldable,Data,Typeable)


makeLenses ''EllipsoidSurface

wgs98 = (6378137,0.00335281)
radiusM a e phi = a*(1 -e^2 )/(1 - e^2*(sin phi)^2)**1.5
radiusN a e phi = a/(1 - e^2*(sin phi)^2)**0.5

e1 = EllipsoidSurface 0.2 0.2 0  (fst wgs98) (snd wgs98)
ecentricity f = sqrt(f*(2-f))


type instance Local EllipsoidSurface = V3
instance Space EllipsoidSurface where
    dim _ _ = 3
    EllipsoidSurface n e h a f |+| V3 dn de dh
        | dh > 0 = EllipsoidSurface (n + dn/(re n + h )) (e + de/(cos n *( rn n + h ))) (h + dh) a f
        | otherwise  = EllipsoidSurface (n + dn/(re n + h + dh )) (e + de/(cos n *( rn n + h + dh ))) (h + dh) a f
        where
            rn = radiusN a (ecentricity f)
            re = radiusM a (ecentricity f)
    EllipsoidSurface n e h a f |-| EllipsoidSurface n2 e2 h2 a2 f2
        | h2 > h =  V3  ((n - n2)*(re n + h2)) ((e - e2)*(rn n + h2)*(cos n)) (h - h2)
        | otherwise =  V3  ((n - n2)*(re n + h)) ((e - e2)*(rn n + h)*(cos n)) (h - h2)
            where
            rn = radiusN a (ecentricity f)
            re = radiusM a (ecentricity f)


