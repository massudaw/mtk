module Linear.Diagonal where

import Control.Applicative
import Linear.V1
import Linear.V2 
import Linear.V3

class Eye f where
    eye :: Num a => f a -> f ( f a)

ident :: (Num a ,Applicative s,Eye s) => s (s a)
ident = eye (pure 1)


instance Eye V1 where
    eye (V1 x ) = V1 (V1 x )
instance Eye V2 where
    eye (V2 x y ) = V2 (V2 x 0 ) (V2 0 y)

instance Eye V3 where
    eye (V3 x y z ) = V3 (V3 x 0 0 ) (V3 0 y 0 ) ( V3 0 0 z)
