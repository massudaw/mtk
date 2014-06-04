{-# LANGUAGE
  DeriveDataTypeable ,
  DeriveFoldable ,
  UndecidableInstances,
  DataKinds,
  ConstraintKinds,
  KindSignatures,
  ScopedTypeVariables,
  RankNTypes,
  TypeOperators,
  DeriveTraversable,
  GeneralizedNewtypeDeriving,
  ImpredicativeTypes,
  TypeFamilies,
  StandaloneDeriving,
  FlexibleContexts,
  DeriveFunctor,
  TypeSynonymInstances,
  FlexibleInstances,
  DeriveGeneric,
  NoMonomorphismRestriction
  #-}


-- Generate a dataset
-- Using a the bicicle model

import GHC.Exts
import Space.Class
import GHC.Generics hiding(V1)
import Control.Monad.Trans.State
import Text.PrettyPrint.GenericPretty
import System.Directory (doesFileExist)
import qualified MultiLinear.Class as M
import qualified System.IO as IO
import Data.Distributive
import Control.Monad
import Data.Monoid hiding(Product)
import Data.Ublox.Ublox
import Solver.RungeKutta
import Linear.V3
import Linear.V2
import Vectorization.Instances
import Linear.V1
import Linear.Metric
import Scaling.Time
import Sensor.Razor9DOF
import qualified Linear.Vector as V
import qualified Data.Map as M

import Debug.Trace
import Display.Cube
import Data.List(transpose)
import Local
import Rotation.SO3
import Rotation.Class
import Exponential.Class
import Kalman
import Prelude hiding(sequence,readFile,writeFile)
import As

import Display.Main

import Data.Data
import Space.SE3
import Linear.Matrix
import Linear.Vector
import Debug.Trace


import Data.Foldable(toList,Foldable)
import Control.Lens
import Control.Monad (when,liftM,replicateM,(>=>))
import Control.Applicative
import Data.Distributive
import Data.Functor.Product
import Data.Functor.Compose

import System.IO
import System.Environment

import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P 
import Pipes.Safe
import Attitude 

--import Ellipsoid

import CartesianProduct
import SemiProduct
import Foreign.Cholesky

main = do
    process

countPipe i =  do
    input <- await 
    liftIO $ print i
    yield input
    countPipe (i+1)

process = do 
    let flog = "result/" ++ "serial" 
	gpsPort = "/dev/ttyUSB0"
    let process solvers visualizer = do
            (input , output) <- spawn (Bounded 10)
            forkIO $ runSafeT  ( runEffect $ openIMUSafe  gpsPort >-> parseData >-> countPipe 0 >-> (toOutput input >> return undefined) >> return ()) 
            mapM_ (\solver ->  runSafeT $ runStateT (  runEffect ((fromInput output >> return undefined  )>-> P.map IMUPacket >-> solver >-> (P.map (^._1._1.orientation))  >-> visualizer )) init0 >> return ()) solvers
            return ()
    process [(forever imuSolver)] glSafe  

{-
        "gl-serial" -> runSafeIO $ runProxy $ runEitherK  $ openIMUSafe f >-> (try .) (mapD IMUPacket >-> imuPipe f ) >-> tryK(mapD (^._1._1.orientation)) >-> glSafe
        "gl-file" -> runSafeIO $ runProxy $ runEitherK  $ readLogSafe f >-> tryK(mapD IMUPacket >-> imuPipe f) >-> tryK(mapD (^._1._1.orientation)) >-> glSafe
        "log-file" -> runSafeIO $ runProxy $ runEitherK  $ readLogSafe f >-> tryK(mapD IMUPacket >-> imuPipe f) >-> writeLogSafe flog
        "log-serial" -> runSafeIO $ runProxy $ runEitherK  $ openIMUSafe f >-> tryK(mapD IMUPacket >->imuPipe "serial") >-> writeLogSafe flog
         "log-serialD" -> do(input,output) <- spawn Single 
			   forkIO $ runSafeIO $ runProxy $  runEitherK $ openIMUSafe f >-> tryK(mapD IMUPacket >-> sendD input )
			   forkIO $ runSafeIO $ runProxy $  runEitherK $ readGPS gpsPort  >-> tryK(mapD GPSPacket >-> sendD input )
			   runSafeIO $ runProxy $  runEitherK $ tryK(recvS output >-> imuPipe "serial") >-> writeLogSafe flog 
	"sensors-log" -> do(input,output) <- spawn Single 
			   forkIO . runSafe $ openIMUSafe f >-> tryK(mapD IMUPacket >-> sendD input )
			   forkIO . runSafe $ readGPS gpsPort  >-> tryK(mapD GPSPacket >-> sendD input )
			   runSafe $ (try .) (recvS output) >-> writeLogSafe flog 
	"log-filter" -> do runSafe $ readSensorSafe flog >-> tryK(imuPipe f >-> mapD (^._1._1.orientation) ) >-> glSafe
-} 

runSafe = runSafeT . runEffect


imuPipe file  = init 
    where 
        init = do
            let stateFile = file ++ ".prior" 
            existFile <- liftIO $ doesFileExist stateFile
            if existFile
                then do
                    liftIO $ putStrLn "File Exist"
                    sfile <- liftIO $ IO.readFile stateFile
                    let st0 = read sfile :: (Position Double , Covariance Position Double)
                    liftIO $ putStrLn $ "using file Covariance"
                    liftIO $ getChar
                    lift $ put st0
                    sqrtImuKalman stateFile 
                else do
                    liftIO $ putStrLn $ "using initial Covariance"
                    --lift $ putStrLn $ prettyState (0,0) (snd init0)
                    lift $ put init0 
                    sqrtImuKalman stateFile 

imuSolver =  do
    IMUPacket imu <- await 
    lift $ imuInit imu
    s <- lift $ get 
    yield  (s,undefined)

gpsInit (GPSTimed t i) = do 
	let position = ecefPos  i
            posLen = _1.se3.sndPL1
	modify (posLen._x.~ (ecefX position))
	modify (posLen._y.~ (ecefY position))
	modify (posLen._z.~ (ecefZ position))
 
imuInit (t0,mi@(IMU accR gyrR magR)) = do 
        let
            accUn = linScaleA (s0 ^. accCalib . deCompose) accR
            gyrUn = linScaleA (s0 ^. gyroCalib . deCompose) gyrR
            magUn = linScaleA (s0 ^. magCalib . deCompose) magR
            acc = fmap (*9.81) $ fmap (/norm accUn) accUn
            mag = fmap (/norm magUn) magUn
            angles = anglesMeasure (IMU acc gyrUn mag)
        modify (_1.time.~ (V1 t0))
        modify (_1.orientation.~(rotation angles))

sqrtImuKalman file = init where
    init = do
        x <- await 
	case x of
	    IMUPacket imu -> do lift $ imuInit imu
				initGPS
	    GPSPacket gps -> do lift $ gpsInit gps 
			        init	
    initGPS = do
	x <- await 
	case x of
	    IMUPacket imu -> do iterIMU imu
	    GPSPacket gps -> do lift$ gpsInit gps 
				loop	
        initGPS 
    loop = do
	x <- await 
	case x of
	    IMUPacket imu -> do iterIMU imu
	    GPSPacket gps -> do	iterGPS gps
	loop
    iterGPS mi@(GPSTimed t i) = do
	let	posNed = ecefPos i 
		pos = V3 ( ecefX posNed) (ecefY posNed) (ecefZ posNed) 
	lift $ positionMeasurement pos
        (lift $ get) >>= \s -> yield (s , undefined)


    iterIMU (dt,mi@(IMU accR gyrR magR))= do
        --magnetometerMeasure magR
        --magnetometerMeasureGyro magR
        --magnetometerMeasureCalib magR
        snew <- lift $ get
        let  accel = linScaleA (s0 ^. accCalib . deCompose) accR
        when (abs (norm accel ) - 9.81 >  5.0 ) $ do
            lift $ gyroscopePrediction dt gyrR 
            --liftIO $ putStrLn "Acceleration Outlier"
        when (abs (norm accel ) - 9.81 <  5.0 ) $ do
            --gyroscopePrediction dt gyrR 
            lift $ accelerometerPrediction dt gyrR accR
            lift $ accelerometerMeasureCalib accR
            --accelerometerMeasureGyro accR
        s <- lift $ get 
        yield (s , mi)


accelerometerPrediction :: Monad m => Double -> V3 Double -> V3 Double -> 
             StateT (Position Double,
           M  (Local Position )
             Double)
            m  ()
accelerometerPrediction dt gyrR accR = do
        x <- get
        let 
            scaled = sensorScaling gyrR accR 
            fun :: (V1 :|: SE3 :|: V3 :|:  LinearParamV3 :|: LinearParamV3 :|: V1 ) Double -> (V1 :|: SE3 :|: V3 ) Double
            fun (t :|:  a  :|: v :|: gyrscale :|: accscale :|: g) = rkf45_aux_tan (positionTransition (scaled gyrscale accscale) g )  dt  (t :|: a :|: v )
 	    timeVar = fstPL1 |:| sndPL1 . fstPL1 |:| sndPL2  . fstPL1
        modify ((alongside (positionTransitionFocus . alongState' timeVar )(alongMatrix positionTransitionFocus . alongState timeVar ) ) %~ sqrtPrediction fun positionCovariance )
        return ()

positionCovariance :: Covariance (Local (V1 :|: SE3 :|: V3 )) Double
positionCovariance  = liftIdent (1e-5 :|: (pure gyroNoise  :|: 1e-3 ) :|: 1e-3 )
gyroscopePrediction :: Monad m => Double -> V3 Double  -> 
           StateT  (Position Double,
           M  (Local Position)
             Double)
            m  ()
gyroscopePrediction dt gyrR = do
	x <- get
 	let gyrFun = (x ^. _1 . gyroCalib)
            fun :: (V1 :|: SO3 :|: LinearParamV3 ) Double -> (V1 :|: SO3 :|: LinearParamV3) Double
            fun (t :|: a  :|: scale ) = (t' :|: a' :|: scale) 
		where (t' :|: a' ) = rkf45_aux_tan (gyroTransition gyrR scale)  dt  (t :|: a)  
        --modify ((alongside (gyr . along timeO )(alongMatrix gyr . alongtimeO  )) %~ sqrtPrediction fun gyroscopeCovariance  )
        modify ((alongside (gyr ) (alongMatrix gyr)) %~ sqrtPrediction fun (gyroscopeCovariance dt) )
        return ()

timeO = fstPL1 |:| sndPL1 . fstPL1 

positionMeasurement ::  Monad m  => V3 Double  -> 
            StateT (Position Double,
           M  (Local Position)
             Double)
          m  ()
positionMeasurement posR =
        modify (alongside position (alongMatrix position)  %~ sqrtMeasure id positionMeasureCovariance posR )


accelerometerMeasure ::  Monad m  => V3 Double  -> 
             StateT (Position Double,
           M  (Local Position)
             Double) m ()
accelerometerMeasureCalib accR =
        modify (alongside accCalibration (alongMatrix accCalibration )  %~ sqrtMeasure gravityFieldCalib accelerometerCovariance accR)

accelerometerMeasureGyro accR = do
        x <- get 
 	let gravFun = gravityFieldGyro (x ^. _1 . accCalib)
        modify (alongside (orientation|:| gyroCalib) (alongMatrix (orientation |:| gyroCalib))  %~ sqrtMeasure gravFun accelerometerCovariance accR)


accelerometerMeasure accR = do
        x <- get 
 	let gravFun = gravityField (x ^. _1 . (accCalib |:| gyroCalib))
        modify (alongside orientation (alongMatrix orientation)  %~ sqrtMeasure gravFun accelerometerCovariance accR)

magnetometerMeasureCalib magR =
        modify (alongside magCalibration (alongMatrix magCalibration) %~ sqrtMeasure magneticFieldCalib magneticCovariance magR)

magnetometerMeasureGyro magR = do
        x <- get 
 	let magFun = magneticFieldGyro (x ^. _1 . magCalib )
	    len =  alongside (orientation |:| orientationOffset |:| gyroCalib) (alongMatrix (orientation |:| orientationOffset |:| gyroCalib))
        modify (len %~ sqrtMeasure magFun  magneticCovariance magR)


magnetometerMeasure magR = do
        x <- get 
 	let magFun = magneticField (x ^. _1 . (magCalib |:| gyroCalib))
        modify (alongside mag (alongMatrix mag) %~ sqrtMeasure magFun  magneticCovariance magR)

-- Initial Conditions 
--
gyroNoise :: Double
gyroNoise = 0.50*pi/180 
accNoise = 9.81/1000/(accScale ^. _x )
magNoise = 0.5*pi/180/(magScale ^. _x )


accelerometerCovariance , magneticCovariance :: Covariance V3 Double
accelerometerCovariance  = liftIdent ( pure  accNoise )
magneticCovariance  = identM (pure  magNoise)
positionMeasureCovariance = identM (pure  0.00001)

gyroscopeCovariance :: Double -> Covariance (V1 :|: V3 :|: LinearParamV3 ) Double
gyroscopeCovariance  dt = liftIdent (1e-5 :|: pure (dt*gyroNoise) :|: (fmap (*dt)sensorParam) )
    where sensorParam =  Compose $ V3 gc gc gc
 	  gc = 1e-4 :|: 0.1
    

attitudeCovariance :: Covariance Position Double
attitudeCovariance = liftIdent ( 1e-5 :|: (pure gyroNoise :|:  1e-3) :|: 0.1 :|: pure 1e-2 :|: accelerometer :|: sensorParam :|: sensorParam :|: 0.1)
    where sensorParam =  Compose $ V3 gc gc gc
 	  gc = V1 1e-6 :|: V1 1e-2
    	  accelerometer =  Compose $ V3 gc1 gc1 gc1
 	  gc1 = V1 1e-4 :|: V1 1e-2

{-
prettyState =  (forever loop )
  where loop = do 
	s@(((t :|: ( a:>:p ) :|:  v :|: o  :|: aa :|: ma :|: ga :|: g ),cov),imu@(IMU acc gyr mag)) <- await
	let spacedPrint label = lift . putStrLn . (label++) . foldl1 (\x y -> x ++ " \t"++ y  ) .  map show 
	let spacedPrintPretty label = lift . putStrLn . (label++) . prettyStateMatrix
	lift $ putStrLn $ " ############# Break ##############"
	lift $ print imu 
	spacedPrintPretty "Raw Angles: \n" (anglesMeasure imu )
	spacedPrintPretty "Time: \n"$  distribute (gaussianInterval t ( join (cov ^. alongMatrix time)))
	spacedPrintPretty "Filtered Angles: \n"$  distribute (gaussianInterval (angles a ) ( join (cov ^. alongMatrix orientation )))
	spacedPrintPretty "Gyro Calibration: \n"  $ toList $ distribute (gaussianInterval ga  ( join (cov ^. alongMatrix gyroCalib )))
	spacedPrintPretty "Gyro Calibration: \n"  $ toList $ distribute (gaussianInterval (logM o)  ( join (cov ^. alongMatrix orientationOffset )))
	--lift $ putStrLn $ prettyStateMatrix (0,0) cov 
	spacedPrintPretty "Accelerometer Calibration: \n"$ toList $ distribute (gaussianInterval aa  (join (cov ^. alongMatrix accCalib )))
	spacedPrintPretty "Magnetometer Calibration: \n"$ toList $ distribute  (gaussianInterval ma  (join (cov ^. alongMatrix magCalib )))
	yield s 

-}


gaussianInterval m d =  GaussianInterval (m |+| d) ( m |+| (fmap negate d))

data GaussianInterval a
  = GaussianInterval
  { lowerSigma ::  a
  , upperSigma ::  a
  }deriving(Read,Show,Foldable,Functor,Generic,Traversable)
 
data Gaussian   a
  = Gaussian
  { mean ::  a
  , deviation ::  a
  }deriving(Read,Show,Foldable,Functor,Generic,Traversable)
    
instance  (Out a) => Out (Gaussian  a)
instance  (Out a) => Out (GaussianInterval  a)
deriving instance Generic (V2 a)
deriving instance Generic (V3 a)
deriving instance Generic (V1 a)
instance  Out a => Out (V2 a)
instance  Out a => Out (V1 a)
instance  Out a => Out (V3 a)
{-
		
collectIMUData g = do
   hg <- openFile g WriteMode 
   hPutStrLn hg$ csvwords ["time","roll","pitch","yaw"] 
   let fun (x,y) = V1 x :|: anglesMeasure y
   --runProxy $ runEitherK  $ openIMUSafe "/dev/ttyUSB0" >-> foldr1P (\(x,y) (i,j)-> (x+i,y)) (0,undefined) >-> mapD fun  >->  showFoldableAsCsv >-> hPutStrLnD hg 
   return ()

reprocessAll = do 
	let files = map ("calibrate/"++) ["yaw","pitch","roll"]
	    logs = map (++ ".log") files
	    csv = map (++ ".csv") files
	mapM_ (uncurry buildCsv) $ zip logs csv
buildCsv f g= do
    hf <- openFile f ReadMode
    hg <- openFile g WriteMode 
    --hPutStrLn hg$ csvwords ["accx","accy","accz","gyrox","gyroy","gyroz","magx","magy","magz"]
    hPutStrLn hg$ csvwords ["roll","pitch","yaw"] 
    runProxy ((\h -> readLog' hf)  >-> mapD (anglesMeasure . snd )  >->  showFoldableAsCsv >-> hPutStrLnD hg )

-}

s0 :: Position Double
s0 = 0 :|: ((expM 0 :>: V3 0 0 0 )   :: SE3 Double) :|: 0 :|: expM 0.2 :|: accLin :|: magLin :|: gyroLin :|: 0

init0 =(s0, fmap(fmap (*1)) attitudeCovariance)
