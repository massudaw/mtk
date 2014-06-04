{-# LANGUAGE StandaloneDeriving,ScopedTypeVariables,FlexibleContexts,MultiParamTypeClasses,TypeOperators,FlexibleInstances,DeriveFunctor,DeriveFoldable,DeriveTraversable,GeneralizedNewtypeDeriving,DeriveDataTypeable,TypeFamilies #-}
module Space.Class where
import Linear.V1
import Linear.V2 hiding(_x)
import Linear.V3 hiding(_x)
import Linear.V4
import Linear.Quaternion
import Linear.Matrix
import Linear.Vector
import Linear.Metric
import qualified Linear.Vector as Vector
import Data.Distributive
import Data.Functor.Compose
import Control.Lens hiding(Action)
import Control.Applicative
import Data.Foldable hiding(product,sum)
import Prelude hiding(foldr1)

import CartesianProduct
import SemiProduct


import Data.Data
import Local


deriving instance (Eq (f (g a)) ,Eq (g a)) => Eq (Compose f g a)
deriving instance (Ord (f(g a)) ,Ord (g a)) => Ord (Compose f g a)

class (Additive f,Applicative f,Metric f ,Traversable f,Distributive f) => R f where
    dimM :: f a -> Int


instance R V1  where
	dimM _ = 1

instance R V2  where
	dimM _ = 2

instance R V3 where
  dimM _ = 3

instance R V4 where
  dimM _ = 4

instance (R x , R y) => R (x :|: y) where
    dimM (x  :|: y) = dimM x + dimM y
    

instance (R x , R y) => Metric (Compose x y) where
    dot (Compose x) (Compose k)= quadrance (liftA2 dot x k)

instance (R x , R y) => Metric (x :|: y) where
    dot (x :|: y) (k :|: l) = dot x k + dot y l

class (R (Local b)) => Space b where
  dim :: b a -> (Local b) a  -> Int
  dim  _  = dimM
  (|+|) :: (RealFloat a,Floating a, Eq a ,Num a) => b a -> Local b a  -> b a
  (|-|) :: (RealFloat a,Floating a, Eq a ,Num a) => b a -> b a -> Local b  a

instance Space V1  where
  dim  _ _= 1
  (|+|) = (+)
  (|-|) = (-)

instance Space V2  where
    dim _ _= 2
    (|+|) = (+)
    (|-|) = (-)

instance Space V3  where
    dim _ _= 3
    (|+|) = (+)
    (|-|) = (-)

instance Space V4 where
    dim _ _= 4
    (|+|) = (+)
    (|-|) = (-)


instance (Applicative f, Applicative g ) => Additive (Compose f g ) where
    zero = pure 0

instance (Space x,Space y) => Space (x :|: y) where
    dim (x :|: y) (l :|: k)  =   dim x l  + dim y k
    (x1 :|: x2) |+| (d1 :|: d2) = (x1 |+| d1) :|: (x2 |+| d2)
    (x1 :|: x2) |-| (d1 :|: d2) = (x1 |-| d1 ) :|: ( x2 |-| d2)

(*^^) x = fmap (x *^)

instance (Traversable x,R x , R y) => R (Compose x y) where
    dimM (Compose x) = foldr1 (+) $ fmap dimM x


type instance Local (Compose f g) = Compose  f (Local g)

instance(Traversable f,R f,Space g) => Space (Compose f g) where
    (Compose f) |+| (Compose df) = Compose $ liftA2 (|+|) f df
    (Compose f) |-| (Compose df) = Compose $ liftA2 (|-|) f df


