{-# LANGUAGE StandaloneDeriving,FlexibleContexts #-}
import Kalman
import Vectorization
import Control.Applicative
import Space 
import Data.Functor.Product
import Linear.V1
import Data.IntervalMap.FingerTree


p0 = point 0 
s0 = singleton  p0 (x0,c0) 
x0 :: Product V1 V1 Double 
x0 = Pair 0  0
c0 :: Product V1 V1 (Product V1 V1  Double )
c0 = Pair (V1 $ Pair 1 0) (V1 $ Pair 0  1) 

f a dt (Pair p v) = Pair (p |+| fmap (*dt) v) (p |+| fmap (*dt) a)
h (Pair p v) =  p
r = V1 0.02
q a dt = liftA (fmap (*a)) $ Pair ( V1 $ Pair (V1 (dt^3) ) (V1 (dt^2/2))) (V1 $   Pair (V1 (dt^2/2)) (V1 dt))
