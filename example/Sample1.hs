{-# LANGUAGE NoMonomorphismRestriction #-}
import Numeric.LinearAlgebra
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.Quaternion
import Linear.Matrix
import Vectorization
import Space 
import Kalman


-- Aided Inertial Navigation -- Example 5.4

pe0 = (V2 (V2 100 0) (V2 0 100))
xe0 = V2 0 0
se0 = (xe0,pe0)


he0 (V2 x _) = V1 x
he1 (V2 _ y) = V1 y
he2 (V2 x y) = V1 (x*0.7 + 0.3*y)
he3 (V2 x y) = V1 (x*0.5 + y*0.5)

m0 = (he0,re0,ye0)
m1 = (he1,re1,ye1)
m2 = (he2,re2,ye2)
m3 = (he3,re3,ye3)

mlist = [m0,m1,m2,m3]

re0 = V1 1
re1 = V1 1
re2 = V1 1
re3 = V1 1

ye0 = V1 10.24
ye1 = V1 21.20
ye2 = V1 13.91
ye3 = V1 14.84

measure' (h,r,y) = measure h r y
sef = foldr measure' se0 mlist

se1 = measure he0 re0 ye0 se0
se2 = measure he1 re1 ye1 se1
se3 = measure he2 re2 ye2 se2
se4 = measure he3 re3 ye3 se3


