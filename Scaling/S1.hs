{-# LANGUAGE TypeFamilies ,MultiParamTypeClasses,DeriveFunctor,DeriveFoldable,DeriveGeneric ,TypeOperators#-}
module Scaling.S1 where

import Space.Class
import Exponential.Class
import Data.Foldable
import Multiplicative.Class
import Data.FMonoid.Class
import Data.Distributive
import Data.Monoid
import Linear.V2
import Linear.V1
import Linear.Vector
import Local
import Data.Functor.Product
import qualified Prelude as Prelude
import Prelude hiding((*))
import SemiProduct

newtype Scale a = Scale {unScale :: a} deriving(Functor,Foldable,Read,Show)

instance Distributive Scale where
   distribute x = Scale (fmap unScale x)
instance Exponential Scale  where
   logM (Scale x) = V1 $ log x
   expM (V1 x) = Scale $ exp x

instance Group Scale where
    mult (Scale x) (Scale y)= Scale (x Prelude.* y)
    invert (Scale x) = Scale (-x )

type instance Local Scale  = V1 

instance Action Scale V1 where
    Scale x |> v =  fmap (Prelude.*x) v 

instance Floating a => Multiplicative (Scale a) where
   one = Scale 1
   Scale x * Scale y =  Scale $ x Prelude.* y
   inversion (Scale x) = Scale $ 1/x

instance Space Scale where
   x |+|  y =  x * expM y 
   x |-|  y =  logM $ inversion y *  x 


