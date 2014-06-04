{-# LANGUAGE TypeFamilies ,DeriveGeneric,DeriveFunctor,TypeSynonymInstances,TypeOperators ,MultiParamTypeClasses #-}
module SemiProduct where

import Local
import Control.Applicative

import Rotation.SO2
import Data.Distributive
import Rotation.SO3
import Linear.V2
import Linear.V3
import GHC.Generics hiding(V1)
import CartesianProduct

import Linear.Matrix
import Linear.V1
import Linear.V3
import Linear.V2
import Linear.Vector
import Linear.Vector

class Action f g  where
   (|>) :: Num a => f a -> g a -> g a

instance Action SO3 V3 where
    SO3 x |> v =  x !* v

instance Action SO2 V2 where
    SO2 x |> v =  x !* v

class Group f where
    mult :: Num a => f a -> f a -> f a
    invert :: Num a => f a -> f a

instance Group V1 where
   mult = (^+^)
   invert =  fmap negate
instance Group V2 where
   mult = (^+^)
   invert =  fmap negate
instance Group V3 where
   mult = (^+^)
   invert =  fmap negate

instance Group SO2 where
   mult (SO2 x ) (SO2 y) = SO2 (x !*! y)
   invert (SO2 r ) = SO2 $ distribute r
instance Group SO3 where
   mult (SO3 x ) (SO3 y) = SO3 (x !*! y)
   invert (SO3 r ) = SO3 $ distribute r

instance (Applicative f ,Applicative g) => Applicative (SemiProduct f g) where
   pure x = pure x :>: pure x
   (x :>:  y) <*> (i :>: j)  = ( x <*> i ) :>: (y <*> j)

instance (Group f , Group g,Action f g ) => Group (f :>: g) where
    mult (f :>: g ) (i :>: j) =  (mult f  i) :>: mult g (f |> j) 
    invert (x :>: y) = (ix :>: (ix |> invert y))
	where ix = invert x

type instance Local (f :>: g) = (Local f) :|: (Local g)

data (f :>: g) a = (:>:) { _sfst :: ! (f a) , _ssnd ::  ! (g a) } deriving(Show,Functor,Read,Generic)

type SemiProduct = (:>:) 
