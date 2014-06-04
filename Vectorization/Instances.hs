{-# LANGUAGE TypeSynonymInstances #-}
module Vectorization.Instances where

import Control.Applicative
import Vectorization
import SemiProduct
import CartesianProduct

instance (Vectorize f ,Vectorize g) => Vectorize (SemiProduct f  g) where
    fromList' =  (:>:) <$> fromList' <*> fromList' 
instance (Vectorize f ,Vectorize g) => Vectorize (CartesianProduct f  g) where
    fromList' =  (:|:) <$> fromList' <*> fromList'


