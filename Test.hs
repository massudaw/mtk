{-# LANGUAGE UndecidableInstances,ScopedTypeVariables,FlexibleContexts,RankNTypes,NoMonomorphismRestriction #-}
import Numeric.LinearAlgebra hiding ((><))
import Control.Applicative
import Foreign.Cholesky
import Linear.Metric
import Data.Functor.Compose
import qualified Data.Foldable as F
import Space.Class
import Linear.Vector
import Linear.V1
import Data.Profunctor
import Data.Monoid
import Linear.V2
import Test.QuickCheck
import Data.Distributive
import Linear.V3
import Linear.V4
import Linear.Quaternion
import Linear.Matrix
import Foreign.Cholesky
import Vectorization
import Control.Lens
import Kalman

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

-- Aided Inertial Navigation -- Example 5.4

pe0 = (V2 (V2 100 0) (V2 0 100))
xe0 = V2 0 (0 :: Double)
se0 = (xe0,sqrtM pe0)
sc0 = (xe0,pe0)

sqrtM = unRight . potrf

qr0 = (V2 (V2 0.1 0 ) (V2 0 0.1))


he0 :: V2 Double -> V1 Double
he0 (V2 x _) = V1 x
he1 (V2 _ y) = V1 y
he2 (V2 x y) = V1 (x*0.7 + 0.3*y)
he3 (V2 x y) = V1 (x*0.5 + y*0.5)

f t a (V2 x y) = (V2 y a )
m0 = (he0,re0,ye0)
m1 = (he1,re1,ye1)
m2 = (he2,re2,ye2)
m3 = (he3,re3,ye3)

mlist = [m0,m1,m2,m3]

re0 :: V1 (V1 Double)
re0 = 1
re1 = 1
re2 = 1
re3 = 1

hn x = (distribute $ V4 (he0 x)(he1 x) (he2 x) (he3 x))^. _x

rn = V4
       (V4 1 0 0  0 )
       (V4 0 1 0 0 )
       (V4 0 0 1 0)
       (V4 0 0 0 1)

V1 yn = distribute (V4 ye0 ye1 ye2 ye3)

ye0 = V1 10.24
ye1 = V1 21.20
ye2 = V1 13.91
ye3 = V1 14.84

measure' (h,r,y) = sqrtMeasure h r y
sef = foldr measure' se0 (reverse mlist)

main = defaultMain tests
tests :: TestTree
tests = testGroup "Tests" [properties ]
properties = testGroup "Property" [quick] -- ,small]

quick = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "x == sqrt x * sqrt x"  propSquare
    , QC.testProperty "measureState " propState    
    , QC.testProperty "measureCovariance" propCovariance
    , QC.testProperty "propMulti" propMulti
    ]
{-
small = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "x == sqrt x * sqrt x"  propSquare
  ]
-}

newtype Square f a = Square {unSquare :: (f (f a) )}

instance (Traversable f ,Show (f (f a)) ) => Show (Square f a ) where 
    show  = show . unSquare 

instance (R f,Floating a , Fractional a ,Num a , Arbitrary a, Arbitrary (f (f a)) ) => Arbitrary (Square f a ) where
    arbitrary = fmap (Square . squareT)  tgen
        where tgen = arbitrary :: Gen (f (f a))
            

instance (Num a ,Arbitrary a )=> Arbitrary (V2 a)  where
    arbitrary = V2 <$> (((+1).abs) <$> arbitrary ) <*> (((+1).abs) <$>  arbitrary)
 
instance (Num a ,Arbitrary a )=> Arbitrary (V1 a)  where
    arbitrary = fmap (pure . (+1) . abs ) arbitrary
    
squareNeg :: (Num a, R f) => f a -> f (f a)
squareNeg v = mult v (fmap negate v) 

propCovariance (Square re0 ) ye1 s1 (Square s2) =  nearEqual (snd sc2) (squareT $ snd se2)
    where
        se2 = sqrtMeasure he1 (sqrtM re0) ye1 (s1,sqrtM s2) 
        sc2 = measure he1 re0 ye1 (s1,s2) 


propSquare :: Square V2 Double -> Bool
propSquare (Square x)  = nearEqual x (distribute sq !*! sq)
    where sq = sqrtM x

propMulti s1 (Square s2) =  nearEqualP t (fst sn1) (fst sn2)   &&   nearEqualP (t+3) (snd sn1) (snd sn2)
    where
        t = 7
        sn2 = sqrtMeasure hn (sqrtM rn) yn (s1,sqrtM s2) 
        m1 (h,y) = sqrtMeasure h (sqrtM re0) y 
        l = reverse [(he0,ye0),(he1,ye1),(he2,ye2),(he3,ye3)]
        sn1 = foldr m1 (s1,sqrtM s2) l 

propState (Square re0 ) ye1 s1 (Square s2) = nearEqual (fst sc2) (fst se2) -- && nearEqual (snd sc2) (squareT $ snd se2)
    where
        se2 = sqrtMeasure he1 (sqrtM re0) ye1 (s1,sqrtM s2) 
        sc2 = measure he1 re0 ye1 (s1,s2) 


nearEqualP t x y = abs (x - y) < eps*(abs x) && abs (y - x) < eps*(abs y) 
    where eps = 10^t*2.220446049250313e-16

nearEqual x y = abs (x - y) < eps*(abs x) && abs (y - x) < eps*(abs y) 
    where eps = 1000000*2.220446049250313e-16

