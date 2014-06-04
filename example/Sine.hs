{-# LANGUAGE NoMonomorphismRestriction, ImpredicativeTypes ,RankNTypes, TypeOperators #-}
import Kalman 
import qualified Data.Vector as V
import Vectorization.Instances
import Local
import Linear.V1
import Data.Distributive
import qualified Data.Foldable as F
import Linear.V2
import Data.Complex 
import CartesianProduct
import System.Random.MWC.Distributions
import Vectorization
import Foreign.Storable
import Foreign.Cholesky
import Space.Class
import Control.Applicative
import Control.Monad
import System.Random.MWC
import Graphics.Gnuplot.Simple
import Statistics.Autocorrelation
import Statistics.Sample
import Data.Functor.Compose
import Foreign.FStorable
import Foreign.FStorable

type Model = V1 :|: V1 :|: V1 :|: V1

predSin :: V1 Double -> Model Double -> Model Double
predSin  dt s@(theta :|: omega :|: amp :|: accel)   = s |+|  ((omega*dt )  :|: 0 :|: (dt*accel ) :|: 0 )
  

projSin :: Model Double -> V1 Double
projSin (theta :|: omega :|: amp :|: accel) = amp * (fmap sin theta )

nx :: Model (Model Double)
nx = diag $ fmap (*1e-3)((1/3) :|: 1 :|: 0.1 :|: 0.1)

getMean :: Functor f => (forall b . g b -> V1 b) -> f (g b , g (g b)) -> f b
getMean s = fmap ( unV1 . s . fst )

getCov :: (Num b ,Functor f )=> (forall b .  g b -> V1 b) -> f (g b , g (g b)) -> f b
getCov s = fmap ( abs .unV1 . s .  unV1 . s . snd )

sigmaBound :: Num b => (forall b . g b -> V1 b) -> [(g b , g (g b))] -> [[b]]
sigmaBound s f =  [zipWith (+) (getMean s f) (getCov s f), getMean s  f,zipWith (-) (getMean s f) (getCov s f)]

sigmas :: Num b => (forall b . g b -> V1 b) -> [[(g b , g (g b))]] -> [[b]]
sigmas s = concat . map (sigmaBound s) 

allSigmas :: Num b => [forall b . g b -> V1 b] -> [[(g b , g (g b))]] -> [[[b]]]
allSigmas s d =  map (flip sigmas $ d) s 

unV1 (V1 x) = x 
execute = do 
   seed <- create  
   let n = 500
   v <- replicateM n (normal 0 0.25 seed) 
   let noisedData = testeSet v
       cleanData = testeSet (replicate n 0)
   let ny :: (V1 (V1 Double))
       ny = 1 
       xo :: Model Double
       xo = 0.1 :|: 0.1 :|: 1 :|: 1e-3 
       alpha = 1e-2
       iteration  r = full (predSin 1) nx projSin ny r 
       backwardIteration = smoothing (predSin 1 ) nx 
       forwards =   scanl (flip iteration)  (xo,nx) (fmap V1 $ testeSet v)
       backwards =  reverse $ scanl1 (flip backwardIteration)  $ reverse forwards 
       project = fmap (unV1 . projSin . fst ) 
       getAmp = fmap ( unV1 . amp . fst )
       getAmpCov = fmap ( unV1 .  amp . unV1 . amp . snd) 
       get x = fmap ( unV1 . x . fst )
   -- print projections
   plotLists [] [testeSet (replicate n 0) ,testeSet v,project backwards  ] 
   -- print all parameters 
   mapM_ (plotLists []) (allSigmas [amp ,accel,omega] [backwards])
   let ampCov = V.fromList noiseSignalBackward 
       noiseSignalBackward = zipWith (-)  cleanData  (project backwards)
       noiseSignalForward = zipWith (-)  cleanData (project forwards)
   -- Print residuals
   plotLists [] [noiseSignalForward , noiseSignalBackward ,v] 
   print $ mean ampCov
   print $ stdDev ampCov
   print $ mean $ V.fromList $  zipWith (-) (getAmp forwards) (getAmp backwards)
   print $ stdDev $ V.fromList $  zipWith (-) (getAmp forwards) (getAmp backwards)

amp :: Model a -> V1 a
amp ( x :|: _ :|: amp :|: _ ) = amp 
accel ( x :|: _ :|: amp :|: accel ) = accel 
omega ( x :|: omega  :|: amp :|: accel ) = omega 
theta ( theta :|: _ :|: amp :|: accel ) = theta 
   
matI :: Num a => Int -> [[a]]
matI n = [ [fromIntegral $ fromEnum $ i == j | i <- [1..n]] | j <- [1..n]]

diag = liftIdent
liftIdent ::(R f,Num a,Storable a,Vectorize f,Functor f,Applicative f)=> f a -> f ( f a)
liftIdent x = fmap (liftA2 (*) x) (fromLists $ matI n )
    where n = dimM x

testeSet v =  zipWith (+)  (map (function.fromIntegral) [1..length v]) v where
  function x =  (1+x/500) * sin(16 * x/500 * pi)
