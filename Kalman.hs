{-# LANGUAGE PackageImports,BangPatterns,StandaloneDeriving,TypeFamilies,FlexibleContexts,FlexibleInstances,TypeOperators,ExistentialQuantification,NoMonomorphismRestriction,DeriveTraversable,DeriveFunctor,DeriveFoldable #-}
module Kalman where
import Space.Class hiding(dim)
import Solver.RungeKutta
import qualified Space.Class as M
import Data.Typeable
import Data.Distributive
import Data.Functor.Compose
import Linear.Matrix hiding(trace)
import qualified Linear.Metric as Metric
import Vectorization
import Control.Applicative
import Data.Maybe
import Data.Foldable (Foldable,foldl')
import qualified  Data.Foldable as F
import Data.Traversable (Traversable,fmapDefault,mapAccumL)
import qualified Data.Traversable as T
import Linear.V0
import Linear.V1
import Foreign.Storable
import Foreign.FStorable
import Foreign.Cholesky
import qualified Numeric.LinearAlgebra as M
import qualified Data.Packed.Matrix as M 
import Local
import Linear.Vector
import CartesianProduct
import Control.Monad.Trans.State

-- Covariance Version

fullM
  :: (RealFloat a, FStorable (Local b),
      Vectorize (Local b), Vectorize (Local b1), Space b,
      Space b1, Cholesky a, M.Field a) =>
     (b a -> b a)
     -> Local b (Local b a)
     -> (b a -> b1 a)
     -> Local b1 (Local b1 a)
     -> b1 a
     -> (b a, Local b (Local b a))
     -> (b a, Local b (Local b a))
fullM f r h q  m s= execState  ( do 
      modify (prediction f r)
      modify (measure h q m)
      --modify (smoothing f r s)
      --modify (prediction f r)
      )  s


full
  :: (RealFloat a, FStorable (Local b2), FStorable (Local b),
      Vectorize (Local b), Vectorize (Local b1), Space b2, Space b,
      Space b1, Cholesky a, M.Field a) =>
     (b2 a -> b a)
     -> Local b (Local b a)
     -> (b a -> b1 a)
     -> Local b1 (Local b1 a)
     -> b1 a
     -> (b2 a, Local b2 (Local b2 a))
     -> (b a, Local b (Local b a))
full f r h q  m s= sm 
  where sp = prediction f r s
        sm = measure h q m sp 
        ss = smoothing f r s sm
        ssp = prediction f r ss
        ssm = measure h q m ssp 

predictionM f = modify . prediction f 
measureM h q  =  modify . measure  h q 
smoothingM f r  = modify . smoothing f r 

prediction
  :: (M.Field a , Vectorize (Local b) ,RealFloat a, 
      FStorable (Local b1), Space b1, Space b, Cholesky a) =>
     (b1 a -> b a)
     -> Local b (Local b a)
     -> (b1 a, Local b1 (Local b1 a))
     -> (b a, Local b (Local b a))
prediction f r (x ,p)= (mean ,pxx )
    where
        (mean,pxx,_,_) = sigmaSamplingCovGain f r (x,p)


measure
  :: (RealFloat a, FStorable (Local b),
      Vectorize (Local b1), Space b, Space b1, Cholesky a, M.Field a) =>
     (b a -> b1 a)
     -> Local b1 (Local b1 a)
     -> b1 a
     -> (b a, Local b (Local b a))
     -> (b a, Local b (Local b a))
measure h q m (x ,p) = ( xb  , pb )
    where
        (mmean,pxx,pxy,k) =sigmaSamplingCovGain h q (x,p)
        xb = x |+| ( k !*  ( m |-| mmean ) )
        pb = p !-! ( k !*! pxx !*! distribute k )

smoothing
  :: (RealFloat a, FStorable (Local b), Vectorize (Local b1),
      Space b, Space b1, Cholesky a,
      M.Field a) =>
     (b a -> b1 a)
     -> Local b1 (Local b1 a)
     -> (b a, Local b (Local b a))
     -> (b1 a, Local b1 (Local b1 a))
     -> (b a, Local b (Local b a))
smoothing f r (x,p) (x1,p1) = (xb ,pb) 
    where 
        (xmean ,pxx,pxy,k) = sigmaSamplingCovGain f r (x,p) 
        xb = x |+| (k !* ( x1 |-| xmean ) )
        pb = p !+! (k !*! ( p1 !-! pxx ) !*! distribute k )

sigmaSamplingCovGain
  :: (RealFloat a,M.Field a ,Vectorize (Local b), 
      FStorable (Local b1), Space b1, Space b, Cholesky a) =>
     (b1 a -> b a)
     ->  (Local b (Local b a))
     -> (b1 a, Local b1 (Local b1 a))
     -> (b a, Local b (Local b a), Local b (Local b1 a),Local b1 (Local b a ))
sigmaSamplingCovGain f r (x,p) = (mean , pxx,pxy,k) 
    where
        spoints = (sigmaPoints1 x p)
        points = fmap f spoints 
        mean = sigmaMean1 points
        pxx = sigmaCov mean points !+! r
        pxy = sigmaCovXY x mean spoints  points
        k =  distribute pxy  !*! inverseM pxx
       

sigmaSamplingCov
  :: (RealFloat a, Show (Local b1 (Local b1 a)),
      FStorable (Local b1), Space b1, Space b, Cholesky a) =>
     (b1 a -> b a)
     ->  (Local b (Local b a))
     -> (b1 a, Local b1 (Local b1 a))
     -> (b a, Local b (Local b a), Local b (Local b1 a))

sigmaSamplingCov f r (x,p) = (mean , pxx,pxy) 
    where
        spoints = (sigmaPoints1 x p)
        points = fmap f spoints 
        mean = sigmaMean1 points
        pxx = sigmaCov mean points !+! r
        pxy = sigmaCovXY x mean spoints  points
       

inverseM = fromLists .  M.toLists . M.inv  . M.fromLists . toLists 

{-
measureDelayed h m (x,p) (xlag,plag) =  (xnew,pxk)
    where
        -- Xlag
        sxlag = sigmaPoints1 xlag plag
        -- X
        sx = sigmaPoints1 x p
        -- XXlag
        pxxlag = sigmaCovXY x xlag sx sxlag
        -- Xk
        xk = x :|: xlag
        pxk = prodSym p plag pxxlag
        sxk = sigmaPoints1 xk pxk
        -- Y
        (sy,ym,py) = sigmaTransform h xlag plag
        pxlagy = sigmaCovXY xk ym sxk sy
        k = (distribute pxlagy) !*! (inverseM py)
        xnew = xk |+| (k !* (m |-| ym) )
-}

prodSym x y xy = prod x y xy (distribute xy)

prod x y xy yx = (liftA2 (:|:) x xy ) :|: (liftA2 (:|:) yx  y)


-- Helpers

square :: (Num a, R f) => f a -> f (f a)
square v = mult v v

squareT v = (distribute v) !*! v

mult x m = fmap (x ^*) m

sigmaMean
  :: (RealFloat a, Traversable t, FStorable t, Space b) =>
       t (b a) -> b a -> b a
sigmaMean p p0= case sigmaMean' p p0 of
                  Just x -> x
                  Nothing -> error $ "sigmaMean nonConvergence"

sigmaMean1 (V1 p0 :|: p )= sigmaMean p p0

sigmaMean'
  :: (RealFloat a, Traversable t, FStorable t, Space b) =>
       t (b a) -> b a -> Maybe (b a)
sigmaMean' p p0 = go 1000 p0
    where
        go ! i ! mik
            | i == 0 = Nothing
            | Metric.quadrance m > 1.0e-14 = go (i-1) mik1
            | otherwise = Just mik1
            where
                mik1 = mik |+| m
                s = F.foldr1 (^+^) $  fmap (|-|mik) p
                m =  s ^/ n
                n = 2 * (fromIntegral $ fsize p)

sigmaCovXY xmean ymean samplex sampley  =  covxy
    where
        xstd = fmap (|-| xmean) samplex
        ystd = fmap (|-| ymean) sampley
        covxy =  fmap (^/ 2) $ F.foldr1 (!+!) (liftA2 mult xstd ystd)

sigmaCov mean sample = cov
    where
        std  = fmap (|-| mean) sample
        cov  =  fmap (^/2) $ F.foldr1 (!+!) (fmap square std)



sigmaPoints1 x p = V1 x :|: points
  where
    m = case potrf p of
            Right x -> x
            Left i -> error $ "cholesky factorization error: sigmapoints1 : " 
    points = fmap (x|+|) $  m  :|: fmap ((-1) *^) m



sqrtPrediction
  :: (Applicative g1, Applicative g, Traversable g, Traversable g1,
      FStorable g, FStorable g1, FStorable (Local b1), Space b,
      Space b1) =>
     (b Double -> b1 Double)
     -> g1 (Local b1 Double)
     -> (b Double, g (Local b Double))
     -> (b1 Double, Local b1 (Local b1 Double))
sqrtPrediction f r (x,s)= (xmean,sk'')
    where
        (y,w0m,w0c,w1) = weigthsGamma (fromIntegral $ fsize s)
        sp = fmap (x|+|) $ fmap (fmap (*y)) s :|: fmap (fmap (*(-y))) s
        sp' = fmap f sp
        x'  = f x
        xmean = sigmaMean1 $ V1 x' :|: sp'
        std  =fmap (|-| xmean) sp'
        sk' = dgeqrf $ distribute $ (fmap (fmap (*sqrt w1)) std):|: r
        sk'' = distribute $  ch1upJ sk' $ fmap (*sqrt w0c) $ x' |-| xmean


weigthsGamma l =  weigths 1 2 0 l

weigths a b k l= (y,w0m,w0c,w1) where
    lam =  a^2 * (l + k ) - l
    y =  sqrt ( l + lam)
    w0m = lam /(l + lam)
    w0c = w0m  +  (1 - a^2 + b)
    w1 = 1 / (2*(l+ lam))


ch1dnR x = unRight . ch1dn  x
sqrtMeasure
  :: (Show (Local b Double),Show (b Double),Applicative g, Traversable g, FStorable g, FStorable (Local b),
      FStorable (Local b1), Space b1, Space b) =>
     (b1 Double -> b Double)
     -> g (Local b Double)
     -> b Double
     -> (b1 Double, Local b1 (Local b1 Double))
     -> (b1 Double, Local b1 (Local b1 Double))
sqrtMeasure h r m (x,s) =  (x'',s''')
    where
{-
        spx = V1 x :|: (fmap (x|+|) $ fmap (fmap (*y)) s :|: fmap (fmap (*(-y))) s )
        wc = V1 w0c :|: pure w1
        --wm = V1 w0m :|: pure w1
        V1 y' :|: _ =  spy
        xmean = sigmaMean1  spx
        stdx  =  liftA2 (*^) wc $ fmap (|-| xmean ) spx
        spy = fmap h spx
        ymean = sigmaMean1  spy
        stdy  =  fmap (|-| ymean) spy
        pxy =   distribute stdx !*!  stdy
        sk' =  dgeqrf $ distribute $ fmap (fmap (*sqrt w1))  stdy :|: r
        sk''= distribute .fromJust  $   ch1up sk' (fmap (*w0c) $ ymean |-| y')
-}
        (ymean,pxx,pxy) = sigmaSampling h r (x,s) 
        k  =  pseudoInverse pxy pxx 
        -- x' = xmean |+| (k !* (m |-| y')  )
        x' = x |+| (k !* (m |-| ymean)  )
        u  = pxx !*! distribute k
        s' = distribute $ F.foldl ch1dnR  (distribute s)  u
        (x'',s''',_) = sigmaSampling id V0 (x',s')
{-      spx' = fmap (x' |+| ) $ fmap (fmap (*y))  s' :|: fmap (fmap (*(-y))) s'
        x'' =  sigmaMean1 $ V1 x' :|: spx'
        stdx' =  fmap (|-| x'') spx'
        s'' = dgeqrf $ distribute $ fmap (fmap (*sqrt w1))  stdx'
        s''' = distribute $ fromJust $ ch1up s'' $ fmap (*sqrt w0c) $ x'' |-| x'
-}

pseudoInverse
  :: (Applicative nhrs, Applicative m, Applicative m1,
      Traversable nhrs, Traversable m, Traversable m1, Distributive m1,
      FStorable nhrs, FStorable m, FStorable m1) =>
     nhrs (m1 Double) -> m (m1 Double) -> nhrs (m1 Double)
pseudoInverse y x  = dgelssR (distribute x )  (dgelssR x  y )



sqrtBackward f p (x,s) (x1,s1) = (xb ,sb) 
    where 
        (xmean ,pxx,pxy) = sigmaSampling f p (x,s) 
        k =  pseudoInverse pxy pxx
        xb = x |+| (k !* (x1 |-| xmean ))
        s1pxx = distribute $ F.foldl (\y-> ch1dnR y)  (distribute s1 )  (distribute pxx )
        u  = s1pxx !*! distribute k
        sb = distribute $ F.foldl  ch1upJ  (distribute s)  u

sigmaSampling h r (x,s) = (ymean,sxx,sxy)
    where 
        (y,w0m,w0c,w1) = weigthsGamma (fromIntegral $ fsize s)
        spx = V1 x :|: (fmap (x|+|) $ fmap (fmap (*y)) s :|: fmap (fmap (*(-y))) s )
        wc = V1 w0c :|: pure w1
        --wm = V1 w0m :|: pure w1
        V1 y' :|: _ =  spy
        xmean = sigmaMean1  spx
        stdx  =  liftA2 (*^) wc $ fmap (|-| xmean ) spx
        spy = fmap h spx
        ymean = sigmaMean1  spy
        stdy  =  fmap (|-| ymean) spy
        sxy =   distribute stdx !*!  stdy
        sk' =  dgeqrf $ distribute $ fmap (fmap (*sqrt w1))  stdy :|: r
        sxx = distribute $ ch1upJ sk' (fmap (*w0c) $ ymean |-| y')

ch1upJ x = fromJust . ch1up x
dgelssR x = unRight . dgelss x

unRight (Right x) = x
unRight (Left x) = error $ "no right: " 



