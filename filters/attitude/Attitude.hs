{-# LANGUAGE
      TypeOperators
    , NoMonomorphismRestriction
  #-}

-- Generate a dataset
-- Using a the bicicle model
module Attitude where

import Data.List(transpose)

import Scaling.S1
import Scaling.Time
import Linear.V1
import Linear.V2
import Linear.V3

import Data.Distributive
import Solver.RungeKutta
import Linear.Metric
import Control.Monad (when)

import Product

import Sensor.Razor9DOF

import Vectorization
import Local
import Exponential.SO3
import Exponential.SO2
import Exponential.Class
import Rotation.SO3
import Rotation.SO2
import Space.SO2
import Space.SO3
import Space.Class
import Kalman
import Prelude hiding(sequence,readFile,writeFile)
import As


import Data.Data
import Linear.Matrix
import Data.Maybe

import System.Environment
import Data.Foldable(toList,Foldable)
import Control.Lens hiding ((|>))
import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.TH
import Control.Monad (liftM,replicateM,(>=>))
import Data.Word
import Data.Traversable
import Control.Applicative
import Data.Distributive
import Data.Functor.Compose

import CartesianProduct
import System.IO
import Data.Time
import Foreign.Storable

import Linear.Vector
import Space.SE3
import SemiProduct
import Ellipsoid

{-
data Filtro 
    = Filtro 
    { _time :: V1 Double
    , _se3 :: SE3 Double
    , _speed :: V3 Double
    , _orientationOffset :: SO3 Double
    , _accCalib :: LinearParamV3 Double
    , _magCalib :: LinearParamV3 Double
    , _gyroCalib:: LinearParamV3 Double
    , _gravityMagnitude :: V1 Double
    }deriving(Show,Read)

makeLenses ''Filtro
-}

position  = se3 .  sndPL1
orientation = se3 .  fstPL1

-- Time 
time = fstPL1
-- Dynamics 
se3 = sndPL1 . fstPL1
orientationOffset = sndPL3 . fstPL1
speed = sndPL2 . fstPL1
-- Sensor Calibration
accCalib = sndPL4 . fstPL1
magCalib = sndPL5 . fstPL1
gyroCalib = sndPL6 . fstPL1
-- Field Parameters
gravityMagnitude = sndPL7 

type Attitude
    = V1 :|: SO3 :|: SO2 :|: LinearParamV3 :|: LinearParamV3 :|: LinearParamV3

type Position
    = V1 :|: SE3 :|: V3 :|: SO2 :|: LinearParamV3 :|: LinearParamV3 :|: LinearParamV3 :|: V1

type GyroscopeAccelerometerTransition = SE3 :|: V3 :|: LinearParamV3 :|: LinearParamV3 :|: V1

sensorScaling gyroRaw accRaw (Compose ggo) (Compose ago )  =  acc :|: gyro
    where
      acc = linScaleA ago  accRaw
      gyro = linScaleA ggo  gyroRaw

positionTransition :: (V3 :|: V3 ) Double -> V1 Double -> ( V1  :|: SE3 :|: V3 ) Double  -> Local (SE3 :|: V3 ) Double
positionTransition (acc :|: gyro) (V1 g) (t :|: (r :>: position ) :|: speed )
    = ( gyro :|: speed ) :|: ((invert r) |> acc ^-^ (V3 0 0 g) ^-^ (skewV3 gyro !* speed))

type GyroTransition = V1 :|: SO3 :|: LinearParamV3
gyr  = time |:| orientation |:| gyroCalib
gyr' = time |:| orientation

type MagnetometerMeasure = SO3 :|: SO2 :|: LinearParamV3 :|: LinearParamV3
magCalibration  = orientation |:| orientationOffset |:| magCalib |:| gyroCalib
mag = orientation |:| orientationOffset  

type AccelerometerMeasure = SO3 :|: LinearParamV3 :|: LinearParamV3
acc  = orientation 
accCalibration  = orientation |:| accCalib |:| gyroCalib

positionMeasure = position 

positionTransitionFocus = time |:| se3 |:| speed |:| accCalib |:| gyroCalib |:| gravityMagnitude


matI :: Num a => Int -> [[a]]
matI n = [ [fromIntegral $ fromEnum $ i == j | i <- [1..n]] | j <- [1..n]]

liftIdent ::(R f,Num a,Storable a,Vectorize f,Functor f,Applicative f)=> f a -> M f a
liftIdent x = fmap (liftA2 (*) x) (fromLists $ matI n )
    where n = dimM x

identM ::(R f,Num a,Storable a,Vectorize f,Functor f,Applicative f)=> f a -> M f a
identM x = fmap (liftA2 (*) (liftA2 (*) x x)) (fromLists $ matI n )
    where n = dimM x


backScaleA = liftA2 backscale

linScaleA = liftA2 linscale

gyroTransition ::  V3 Double -> LinearParamV3 Double  -> (V1 :|: SO3 ) Double -> Local SO3  Double
gyroTransition gyroRaw  (Compose ggo) (t :|: SO3 r )
    =   fmap negate $ linScaleA ggo  gyro 
    where  gyro =  gyroRaw

magneticFieldGyro (Compose mgo) ((SO3 r):|: (SO2 od ) :|: ggo )
    -- = backScaleA mgo (r !* (odV3 !* V3 1 0 0 )  )
    = backScaleA mgo (r !* (V3 1 0 0 )  )
    where odV3 = alongMatrix _zx .~ od $ identV3 


magneticFieldCalib ((SO3 r):|: (SO2 od ) :|:  (Compose mgo):|: ggo )
    = backScaleA mgo (r !* (odV3 !* V3 1 0 0 )  )
    where odV3 = alongMatrix _zx .~ od $ identV3 

northPoleField declination = odV3 !* V3 1 0 0
    where odV3 = alongMatrix _zx .~ declination $ expM 0 

magneticField (Compose mgo :|: ggo)  ((SO3 r):|: (SO2 od ))
    = backScaleA mgo (r !* (odV3 !* V3 1 0 0 ) )
    where odV3 = alongMatrix _zx .~ od $ identV3

gravity :: Double
gravity  = 9.81

gravityFieldGyro (Compose ago) ((SO3 r):|:  (Compose ggo ))
    = backScaleA ago (r !* V3 0 0 gravity )

gravityFieldCalib ((SO3 r):|: (Compose ago):|: (Compose ggo ))
    = backScaleA ago (r !* V3 0 0 (gravity) )
gravityField ( Compose ago :|: Compose ggo ) (SO3 r)
    = backScaleA ago (r !* V3 0 0 (gravity) )
type Covariance f a= Local f (Local f a )


anglesMeasure (IMU acc _ mag)= angles
    where
            ax = atan2 (acc ^. _y) (acc ^. _z )
            ay = atan2 (-acc ^. _x) (sqrt $ (acc ^._y)^2 + (acc ^. _z )^2)
            magr = distribute r !* mag
            SO3 r = rotation (V3 ax ay 0)
            az = atan2 (- magr ^. _y) (magr ^. _x )
            angles = V3 ax ay az


-- Product Lenses


deCompose =  lens get set
    where set (Compose x ) y = Compose y
          get  (Compose x) = x


fstPL = _ffst 
sndPL = _fsnd 
sndPL1 = sndPL 
fstPL1 = fstPL 
fstPL2 = fstPL1 . fstPL1
fstPL3 = fstPL1 . fstPL2
fstPL4 = fstPL1 . fstPL3
fstPL5 = fstPL1 . fstPL4
fstPL6 = fstPL1 . fstPL5

sndPL2 = sndPL1 . sndPL1
sndPL3 = sndPL1 . sndPL2
sndPL4 = sndPL1 . sndPL3
sndPL5 = sndPL1 . sndPL4
sndPL6 = sndPL1 . sndPL5
sndPL7 = sndPL1 . sndPL6


v3toTuple (V3 x y z) = (x,y,z)

degrees x = x*180/pi

