{-# LANGUAGE DoAndIfThenElse,RankNTypes,FlexibleInstances,NoMonomorphismRestriction,TypeOperators,DeriveFoldable,TypeFamilies,DeriveFunctor,DeriveDataTypeable,TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Sensor.Razor9DOF where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import System.Hardware.Serialport
import System.IO
import Data.Time
import Data.Time.Clock.POSIX
import Control.Monad
import Data.Monoid
import Pipes
import qualified Pipes.Prelude as P
import Pipes.Safe
import Pipes.Concurrent
import System.IO
import Control.Monad.Trans.State
import Control.Applicative
import Linear.V3
import Linear.V1
import Vectorization
import Control.Lens
import Control.Lens.Setter
import Control.Lens.TH
import Data.List as List
import Data.Distributive
import qualified System.IO as IO
import Prelude hiding(writeFile)


-- Derivables
import Data.Data
import Data.Foldable (Foldable)
import CartesianProduct
import Scaling.S1 
import Pipes.Safe.Prelude as SF

import qualified Data.Time as Time

import Space.Class
import Local
import Data.Foldable (foldMap, )
import Data.Monoid (mappend, )
import Data.Functor.Compose
import Linear.V2
import Data.Ublox.Ublox


linscale ( Scale s :|: V1 o) x = (x + o)*s

backscale (Scale s :|: V1 o) x = x/s - o


type LinearParamV3 = Compose V3 (Scale :|: V1 ) 

data Sensors
   = IMUPacket (Double,IMU Double)
   | GPSPacket (GPSTimed NEDSolution)
   deriving(Show,Read)
 

instance Vectorize IMU where
  fromList' = IMU <$> fromList' <*> fromList' <*> fromList'

accLin :: LinearParamV3 Double
accLin = Compose $ liftA2 offScale (accScale)(-accOffset)

offScale x y = Scale x :|: V1 y
gyroLin :: LinearParamV3 Double
gyroLin = Compose $ liftA2 offScale (gyroScale)(-gyroOffset)

magLin :: LinearParamV3 Double
magLin = Compose $ liftA2 offScale (magScale)(-magOffset)

csvFoldable :: (Show a ,Foldable f) => f a ->  String
csvFoldable =  foldl1 (\x y-> x ++ ',': y ) . map show . toList 

instance Distributive IMU where

data IMU a
    = IMU { accRaw :: V3 a
          , gyroRaw :: V3 a
          , magRaw :: V3 a
          }deriving(Show,Eq,Foldable,Typeable,Data,Functor,Read)



parseData
  :: MonadIO m => Proxy () [Char] () (Double, IMU Double) m b
parseData =  forever loop where
     loop  = do
         x <- await 
         let delimiter = wrapList . tail . List.takeWhile (/= '#') . List.dropWhile (/= '$')
             wrapList x = read $  "[" ++ x ++ "]"
         t<-liftIO $ getCurrentTime
         yield (realToFrac $ utcTimeToPOSIXSeconds t ,( fromList (delimiter x ) :: IMU Double ) )


scale = IMU 3.822325e-2  0.0195 1.1627e-2
offset = IMU (V3 17 (-20) 2 ) (V3 (-144.177 ) (-19.732) (-3.36)) (V3 (-36.7) (6.0) (16.5))

accScale,magOffset ,magScale :: V3 (Double)
accScale = accRaw scale 
accOffset = accRaw offset
magScale =  magRaw scale
magOffset =  magRaw offset
gyroScale :: V3 Double
gyroScale =  pure $ pi/14.375/180
gyroOffset =gyroRaw offset


instance FromField a => FromRow (V3 a) where
    fromRow = V3 <$> field <*> field <*> field
instance FromField a => FromRow (IMU a ) where
    fromRow  = IMU <$>  fromRow <*> fromRow <*> fromRow 
        where 
    

instance ToRow (IMU Double) where
    toRow  = map toField . toList 

testQuery = do
    conn <- connect (defaultConnectInfo {connectDatabase = "gps"}) 
    query_ conn selectIMU :: IO [IMU Double]
    
readQuery database query =  bracket 
   (connect (defaultConnectInfo {connectDatabase = database }) )
   close
   (readDB query) 


writeQuery database query =  bracket 
   (connect (defaultConnectInfo {connectDatabase = database }) )
   close
   (writeDB query) 

insertIMU = "insert into imuLog (timestamp,accx,accy,accz,gyrox,gyroy,gyroz,magx,magy,magz) values (?,?,?,?,?,?,?,?,?,?)"
selectIMU = "select  timestamp,accx,accy,accz,gyrox,gyroy,gyroz,magx,magy,magz from imuLog order by timestamp"

readDB
  :: (FromRow r, MonadIO m) =>
     Query -> Connection -> Producer' r m () 
readDB query conn = P.concat <-< loop where
   loop = do 
        yield =<< liftIO ( query_ conn query  )
    

writeDB
  :: (ToRow r, MonadIO m) =>
     Query -> Connection -> Consumer' r m () 
writeDB query conn = loop where
   loop =   do  liftIO . execute conn query  =<< await 
                return ()

scaleIMU ()= init
     where init = do
            (t,IMU acc gyr mag) <- await 
            lift $ print acc
            let new =  IMU(liftA2 linscale (getCompose accLin )acc )( liftA2 linscale (getCompose gyroLin )gyr ) (liftA2 linscale (getCompose accLin)mag)
            yield new
            init


--openIMUSafe :: FilePath -> Producer' String (SafeT IO ) ()
openIMUSafe port = bracket 
    (liftIO $ do 
        h <- hOpenSerial port defaultSerialSettings{commSpeed=CS57600}
        hFlush h
        hSetNewlineMode h $ NewlineMode CRLF CRLF
        return h )
    (liftIO  . hClose)
    P.fromHandle 


{-

readLog h =  init where
     init = do
        x <- lift $ hGetLine h
        let mesg = read x ::(UTCTime ,IMU Double)
        put (fst mesg)
        lift $ putStrLn "Initial UTCTime: "
        lift $ print $ fst mesg
        loop undefined
     loop slast = do
        eof <- lift $ hIsEOF h
        if not eof
            then do
                lift $ putStrLn "More Input"
                x<- lift $ hGetLine h
                let mesg = read x ::(UTCTime ,IMU Double)
                initTime <- get
                let newmesg = _1 %~ (\i->  realToFrac $ diffUTCTime i  initTime :: Double) $ mesg
 		put  ( mesg ^. _1) 
                lift $ putStrLn ("Measure: " ++ show newmesg)
                s <- yield newmesg
                loop s
            else do
                let initFile = "state/razor.state" 
                lift $ writeFile initFile (show slast)
                lift $ print slast
                lift $ putStrLn "No More Input"
                lift $ putStrLn "Final UTCTime: "
                get >>= lift . print

-}
