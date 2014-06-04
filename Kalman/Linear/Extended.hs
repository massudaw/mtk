{-# LANGUAGE RankNTypes #-}
module Kalman.Linear.Extended where


import Kalman.Linear.Naive

import Linear.Diagonal
import Control.Applicative
import Linear.Matrix
import qualified Data.Foldable as F
import Linear.Trace
import Linear.Vector
import Data.Distributive
import Data.Traversable
import Numeric.AD
import Numeric.AD.Types

data NonLinearModel s n h g a
    = NonLinearModel
    { transition :: (forall a . Num a => s a -> s a )
    , measurement :: (forall a . Num a => s a -> h a )
    , processNoise :: s (g a)
    , measureNoise :: h (n a)  
    , processNoiseCorrelation :: g (g a)
    , measureNoiseCorrelation :: n (n a)
    , noiseCorrelation :: g ( n a )  
    } 


linearize 
  :: (Num t4 ,Traversable t,  F.Foldable t, F.Foldable t3, Distributive t,
      Distributive t3, Additive t, Additive t3) =>
     NonLinearModel t t1 t t3 t4
     ->  t t4 -> LinearModel t t1 t t3 t4 
linearize (NonLinearModel s m  g b vg vb vgb) x 
    = (LinearModel (jacobian s x) (jacobian m x)  g b vg vb vgb) 

predictionNon
  :: (Num t4 ,Traversable t,  F.Foldable t, F.Foldable t3, Distributive t,
      Distributive t3, Additive t, Additive t3) =>
     NonLinearModel t t1 t t3 t4
     ->   t3 t4 -> (t t4, t (t t4)) -> (t t4, t (t t4))
predictionNon m  u xp@(x,p) 
    = prediction (linearize m x) u xp
