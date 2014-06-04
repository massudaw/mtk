{-# LANGUAGE
  DeriveDataTypeable ,
  DeriveFoldable ,
  ScopedTypeVariables,
  RankNTypes,
  TypeOperators,
  DeriveTraversable,
  GeneralizedNewtypeDeriving,
  ImpredicativeTypes,
  TypeFamilies,
  PackageImports,
  StandaloneDeriving,
  FlexibleContexts,
  DeriveFunctor,
  TypeSynonymInstances,
  FlexibleInstances,
  GeneralizedNewtypeDeriving,
  NoMonomorphismRestriction,
  TemplateHaskell #-}

-- Generate a dataset
-- Using a the bicicle model
module Position where

import System.Directory (doesFileExist)
import qualified MultiLinear.Class as M
import qualified System.IO as IO
import Debug.Trace as D
import Linear.V1
import Data.Distributive
import Linear.V2
import Solver.RungeKutta
import Linear.V3
import Linear.Metric
import Control.Monad (when)
import Sensor.Razor9DOF
import qualified Linear.Vector as V
import Control.Proxy.Prelude.Base hiding(Product)

import Data.List(transpose)
import Local
import Exponential.SO3
import Exponential.Class
import Rotation.SO3
import Space.SO3
import Space.Class
import Vectorization
import Kalman
import Prelude hiding(sequence,readFile,writeFile)
import As


import Data.Data
import Linear.Matrix
import Data.Maybe
import qualified Data.Map as M
import Data.ByteString.Lazy (readFile,writeFile)



import System.Environment
import Data.Foldable(toList,Foldable)
import Data.Vector.Unboxed.Base
import qualified Data.Vector.Unboxed as U
import Control.Lens hiding ((|>))
import Control.Monad (liftM,replicateM,(>=>))
import Control.Applicative
import Data.Distributive
import Data.Functor.Product
import Data.Functor.Compose
import Rotation.SO2
import Space.SO2
import Linear.Vector

import CartesianProduct
import SemiProduct

import Control.Proxy hiding (Product)
import qualified Control.Proxy.Trans.State as State

import Attitude

import Space.SE3
type Position
    = SE3 :|: V3 :|: SO2 :|: LinearParamV3 :|: LinearParamV3 :|: LinearParamV3 :|: V1


type GyroscopeAccelerometerTransition = SE3 :|: V3 :|: LinearParamV3 :|: LinearParamV3 :|: V1


sensorScaling gyroRaw accRaw (Compose ago :|: Compose ggo )  =  acc :|: gyro
    where
      acc = linScaleA ago  accRaw
      gyro = linScaleA ggo  gyroRaw

--positionTransition :: V3 Double -> V3 Double -> Double -> GyroscopeAccelerometerTransition Double -> Local GyroscopeAccelerometerTransition Double
positionTransition (acc :|: gyro) (V1 g) ((position :>: r) :|: speed )
    = ( gyro :|: speed ) :|: ((invert r) |> acc ^-^ (V3 0 0 g) ^-^ (skewV3 gyro !* speed))

