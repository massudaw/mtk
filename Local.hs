{-# LANGUAGE TypeFamilies ,KindSignatures #-}
module Local where

import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4

type family CLocal (f :: *) :: * -> * 
type family Local (f :: * -> * ) :: * -> *

type instance Local V1 = V1
type instance Local V2 = V2
type instance Local V3 = V3
type instance Local V4 = V4

