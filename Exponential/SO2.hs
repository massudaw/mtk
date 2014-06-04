module Exponential.SO2 where

import Exponential.Class

import Rotation.SO2
import SemiProduct

import Linear.V1
import Linear.V2

instance Exponential SO2 where
    logM = logM2 . unSO2
    expM = SO2 . expM2


instance Exponential SH2 where
    logM = logMH2 . unSH2
    expM = SH2 . expMH2

expMH2 :: Floating a => V1 a -> M2 a
expMH2 (V1 d)
    = V2
        (V2 (cosh d) (sinh d))
        (V2 (sinh d) (cosh d))

expM2 :: Floating a => V1 a -> M2 a
expM2 (V1 d)
    = V2
        (V2 (cos d) (-sin d))
        (V2 (sin d) (cos d))

logMH2 :: RealFloat a => M2 a -> V1 a
logMH2 (V2 (V2 x11 _ ) (V2 x21 _ )) = V1 $ atanh2 x21 x11

atanh2 x y = (log (( y + x)/(y -x)))/2

logM2 :: RealFloat a => M2 a -> V1 a
logM2 (V2 (V2 x11 _ ) (V2 x21 _ )) = V1 $ atan2 x21 x11

