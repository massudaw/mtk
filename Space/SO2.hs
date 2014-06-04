{-# LANGUAGE StandaloneDeriving,FlexibleContexts,MultiParamTypeClasses,TypeOperators,FlexibleInstances,DeriveFunctor,DeriveFoldable,DeriveTraversable,GeneralizedNewtypeDeriving,DeriveDataTypeable,TypeFamilies #-}

module Space.SO2 where

import Control.Applicative
import Space.Class
import Linear.V1
import Linear.V2
import Data.Data
import Control.Lens
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)


import MultiLinear.Class
import Exponential.Class
import Exponential.SO2
import Rotation.SO2


instance Space SH2  where
  dim _ _= 1
  x |+| d =  x !*! expM d
  y |-| x = logM (transpose x !*! y)


instance Space SO2  where
  dim _ _= 1
  x |+| d =  x !*! expM d
  y |-| x = logM (transpose x !*! y)


