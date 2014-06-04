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

import Display.Cube
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

import Display.Main

import Data.Data
import Linear.Matrix
import Data.Maybe
import qualified Data.Map as M
import Data.ByteString.Lazy (readFile,writeFile)


import Graphics.Gnuplot.Simple

import System.Environment
import Data.Foldable(toList,Foldable)
import Data.Vector.Unboxed.Base
import qualified Data.Vector.Unboxed as U
import Control.Lens
import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.TH
import Control.Monad (liftM,replicateM,(>=>))
import Data.Word
import Data.Traversable
import Control.Applicative
import Data.Distributive
import Data.Functor.Product
import Data.Functor.Compose
import qualified Numeric.LinearAlgebra as L


import System.IO
import Data.Time
import Data.Time.Calendar

import Control.Proxy hiding (Product)
import qualified Control.Proxy.Trans.State as State

import Space.SE3

import Position
import Attitude


s0 :: Position Double
s0 = (0 `Pair` expM (pure 0)) `Pair` 0 `Pair` 0 `Pair` (expM (V3 0 (-0.35) 0 )) `Pair` accLin `Pair` magLin `Pair` gyroLin `Pair` V1 9.81 `Pair` V1 1

init0 =(0,(s0,100 *^^ gyroCov))

main = do
    f:xs <- getArgs
    let flog = "result/" ++ f
    hLog <- openFile ("calibrate/" ++ f ) ReadMode
    fLog <- openFile flog WriteMode
    t <- getCurrentTime
    configureDisplay
    stop
    hClose fLog
    plot   flog


prettyShow x = do
     D.trace (show $ fst $ snd x) (return ())
     --putStrLn "Covariance : "
     --print (snd $ snd x)

imuPipe file x = do
        let stateFile = "state/" ++  file
        existFile <- lift$ doesFileExist stateFile
        if existFile
            then do
                lift $ putStrLn "File Exist"
                sfile <- lift$ IO.readFile stateFile
                let st0 = read sfile
                State.evalStateP (_2.~ st0 $ init0 ) imuKalman
            else
                State.evalStateP init0 imuKalman


state l = _2 ._1 . l
covariance l = _2 . _2 . l
acc  = orientation |.| accCalib |.| gravity
mag  = orientation |.| orientationOffset |.| magCalib |.| magField
gyr  = orientation |.| gyroCalib

linscaleV3 = liftA2 linscale
--imuKalman ::Proxy p => State.StateP (Double, (Attitude Double,Covariance Attitude Double )) p (Attitude Double) (Double, IMU Double) () (Double,[V3 Double] ) IO ()
imuKalman = init where
    init = do
        slast <- State.get
        (t0,mi@(IMU accR gyrR magR)) <- request (snd slast)
        let
            accUn = linscaleV3 (s0 ^. accCalib.deCompose ) accR
            acc = fmap (*9.81) $ fmap (/norm accUn) accUn
            gyr = linscaleV3 (s0 ^. gyroCalib.deCompose) gyrR
            magUn = linscaleV3 (s0 ^. magCalib.deCompose) magR
            mag = fmap (/norm magUn) magUn
            angles = anglesMeasure (IMU acc gyr mag)
        State.modify (_1 .~ t0)
        State.modify (_2._1.orientation.~(rotation angles))
        loop
    loop = do
        slast <- State.get
        (t,mi@(IMU accR gyrR magR)) <- request (snd slast)
        State.modify (predictionE (positionTransition accR gyrR) t gyroCov )
        State.modify (_2  %~ measure magneticField magneticCov magR)
        snew <- State.get
        let
            accUn = linscaleV3 (s0 ^. accCalib.deCompose) accR
            acc = accUn
        when (abs (norm acc ) - 9.81 <  2.0 ) $ State.modify (_2  %~ measure gravityField accelerometerCov accR)
        () <-respond((snd $ snew , mi),[angles (snew ^. _2._1.orientation), anglesMeasure mi,angles (snew ^. _2._1.orientationOffset )])
        loop



plot file = do
    buff<- System.IO.readFile file
    let convert = map read . lines
        input = convert buff :: [(((Attitude Double ,Covariance Attitude Double),IMU Double ),[V3 Double])]
        gyroLin'  = map ( unCompose .  (^. gyroCalib ) . fst . fst . fst)input
        accLin'  = map ( unCompose .  (^. accCalib ) . fst . fst . fst)input
        gyroRawVal =  map ( gyroRaw  . snd . fst)input
        accRawVal =  map ( accRaw  . snd . fst)input
        scaleparam f = map(toList . (\(Compose x )-> fmap _scale x).f . fst . fst . fst) input
        offsetparam f = map(toList . (\(Compose x )-> fmap _offset x).f . fst . fst . fst) input
        gyroVal  = map (toList . fmap (*0.055) ) $ zipWith (liftA2 linscale) gyroLin' gyroRawVal
        accVal  = map (toList . fmap (*0.055) ) $ zipWith (liftA2 linscale) accLin' accRawVal
        accScale = scaleparam (^. accCalib )
        gyroScale = scaleparam (^. gyroCalib )
        magScale = scaleparam (^. magCalib )
        magOffset= offsetparam (^. magCalib )
        gyroOffset= offsetparam (^. gyroCalib )
        accOffset= offsetparam (^. accCalib )
        grav= map(toList .(^. gravity ) . fst .  fst . fst) input
        magneticField = map(toList .(^. magField ) . fst . fst . fst) input
        angles = map (concat . map toList . snd ) input
    plotListsStyle [Title "Angles"] $ zipWith  (\x y ->(PlotStyle Lines . CustomStyle .(\i -> [LineTitle i]) $ x,y)) ["true-roll","true-pitch","true-yaw","raw-roll","raw-pitch","raw-yaw","offset-roll","offset-pitch","offset-yaw"] $ transpose angles
    plotLists [Title "Magnetometer Scale"] $ transpose $ magScale
    plotLists [Title "Accelerometer Scale"] $ transpose $ accScale
    plotLists [Title "Magnetometer Offset"] $ transpose $ magOffset
    plotLists [Title "Accelerometer Offset"] $ transpose $ accOffset
    plotLists [Title "Gyroscope Offset"] $ transpose $ gyroOffset
    plotLists [Title "Gyroscope Scale"] $ transpose $ gyroScale
    plotLists [Title "Gravity"] $ transpose $ grav
    plotLists [Title "Magnetic"] $ transpose $ magneticField
    plotLists [Title "Gyroscope Integrated"] (transpose $ scanl1 (zipWith(+)) gyroVal )
    plotLists [Title "Accelerometer Integrated"] (transpose $ scanl1 (zipWith(+)) accVal )


degrees x = x*180/pi

