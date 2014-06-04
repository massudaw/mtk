module Rotation.Class where


import Linear.V2
import Linear.V3

data Rotation f a = Rotation (f (f a))

type SO2' = Rotation V2
type SO3' = Rotation V3
