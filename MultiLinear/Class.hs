module MultiLinear.Class where

import Rotation.SO2
import Rotation.SO3
import Exponential.SO3
import qualified Linear.Matrix as M
import Data.Distributive
import Linear.V3

class MultiLinear f where
   (!*!) :: Num a => f a -> f a -> f a
   transpose :: Num a => f a -> f a


r0 = rotation (V3 0.1 0.1 0.1)
r1 = rotation (V3 0.1 0.1 0.1)

instance MultiLinear SH2 where
    SH2 x  !*! SH2 y = SH2 ( x M.!*! y )
    transpose = SH2 . distribute . unSH2


instance MultiLinear SO2 where
    SO2 x  !*! SO2 y = SO2 ( x M.!*! y )
    transpose = SO2 . distribute . unSO2

instance MultiLinear SO3 where
    SO3 x  !*! SO3 y = SO3 ( x M.!*! y )
    transpose = SO3 . distribute . unSO3
