module Multiplicative.Class where

import Data.Monoid
import Data.FMonoid.Class


class Multiplicative a where
   one ::  a
   (*) ::  a -> a ->  a
   inversion :: a -> a 

class  FMultiplicative f where
   fone :: Floating a =>  f a
   (^*^) :: Floating a => f a -> f a  -> f a
   finversion :: Floating a => f a -> f a


