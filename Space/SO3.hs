{-# LANGUAGE StandaloneDeriving,FlexibleContexts,MultiParamTypeClasses,TypeOperators,FlexibleInstances,DeriveFunctor,DeriveFoldable,DeriveTraversable,GeneralizedNewtypeDeriving,DeriveDataTypeable,TypeFamilies #-}

module Space.SO3 where

import Control.Applicative
import Space.Class
import Linear.V3
--import Linear.Vector
import qualified Linear.Matrix as M
import Data.Data
import Control.Lens
import Data.Distributive
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)

import MultiLinear.Class
import Rotation.SO3
import Exponential.Class
import Exponential.SO3

instance Space SO3  where
  dim _ _ = 3
  x |+| d = x !*! expM d
  y |-| x = logM (transpose x !*! y)




