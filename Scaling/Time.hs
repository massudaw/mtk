{-# LANGUAGE TypeFamilies ,DeriveFunctor,DeriveGeneric ,TypeOperators#-}
module Scaling.Time where

import Space.Class
import Exponential.Class
import Multiplicative.Class
import Data.FMonoid.Class
import Data.Monoid
import Linear.V2
import Linear.V1
import Linear.Vector
import Local
import Data.Functor.Product
import MultiLinear.Class
import qualified Prelude as Prelude
import Prelude hiding((*))

newtype Epoch a = Epoch a deriving(Functor,Read,Show)

instance Exponential Epoch where
   logM (Epoch x) = V1 $ log x
   expM (V1 x) = Epoch $ exp x

type instance Local Epoch  = V1 

instance Floating a => Multiplicative (Epoch a) where
   one = Epoch 1
   Epoch x * Epoch y =  Epoch $ x Prelude.* y
   inversion (Epoch x) = Epoch $ 1/x

instance Space Epoch where
   x |+|  y =  x * expM y 
   x |-|  y =  logM $ inversion y *  x 


