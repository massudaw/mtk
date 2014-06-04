{-# LANGUAGE FlexibleContexts , RankNTypes, TypeFamilies ,DeriveFunctor, TypeOperators ,TypeSynonymInstances#-}
module Exponential.Class where

import Local
import Linear.V1
import Linear.V2
import Linear.V3
import Space.Class
import SemiProduct
import CartesianProduct

class CExponential c where
    clogM :: forall a .(Ord a , Floating a ) => c ->   CLocal c a
    cexpM :: CLocal c a -> c 



class Exponential f where
    dimTan ::(R (Local f),RealFloat a ) => f a -> Int
    dimTan = dimM . logM
    logM :: RealFloat a => f a -> Local f a
    expM :: RealFloat a => Local f a -> f a

instance Exponential V1 where
   logM = id
   expM = id
instance Exponential V2 where
   logM = id
   expM = id
instance Exponential V3 where
   logM = id
   expM = id

instance (Exponential f ,Exponential g) => Exponential (f  :|: g) where
   logM (f :|: g) = logM f  :|: logM g
   expM (f :|: g) = expM f  :|: expM g

