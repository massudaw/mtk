{-# LANGUAGE NoMonomorphismRestriction #-}
module Kalman.Linear.Naive where

import Linear.Diagonal
import Control.Applicative
import Linear.Matrix
import qualified Data.Foldable as F
import Linear.Trace
import Linear.Vector
import Data.Distributive

data LinearModel s n h g a
    = LinearModel
    { model :: s (s a)
    , measurement :: h (s a)
    , _processNoise :: s (g a)
    , _measureNoise :: h (n a) 
    , processNoiseCorrelation :: g (g a)
    , measureNoiseCorrelation :: n (n a)
    , noiseCorrelation :: g ( n a ) 
    } 

processNoise (LinearModel _ _ g _ vg _ _ ) = (g !*! vg  !*! distribute g)
measureNoise (LinearModel _ _ _ b _ vb _ ) = (b !*! vb  !*! distribute b)


full l u z = measure l z . prediction l u 

prediction  m@(LinearModel a h g b vg vb vgb ) u (x,p) =  (xp,pp)
    where 
        xp = a !* x  ^+^  g !* u 
        pp = a !*! p !*! distribute a !+! processNoise m 

inverse :: s (s a) -> s (s a)
inverse x = x

measure m@(LinearModel a  h g b vg vb vgb) z (x,p) =  (xm,pm)
    where 
        y =  z ^-^ h !* x
        s = h !*! p !*! distribute h !+! measureNoise m 
        k = p !*! distribute h !*! inverse s
        xm = x ^+^ k !* y
        --pm = (ident !-! k !*! h) !*! p
        pm = (ident !-! k !*! h) !*! p !*! distribute (ident !-! k !*! h ) !+! k !*! vb !*! distribute k
        

rtsmoother (LinearModel a  h g b vg vb vgb) (xn,pn) (xp,pp) (x,p) = (xs,ps)
    where
        c = p !*! distribute a !*!  inverse pp
        ps = p !+! c !*! ( pn !-! pp ) !*! distribute c
        xs = x ^+^ c !* ( xn ^-^ xp)  
        m = p !*! distribute c !+! c !*! ( m !-! a !*! p) !*! distribute c
        
bfsmootner (LinearModel a  h g b vg vb vgb ) y s k (iam,lam,x,p)   = (iamp,lamp,xp,pp)
    where 
        lp = distribute h !*! inverse s !*! h !+! distribute c !*! lam !*! c
        lamp = distribute a !*! lp !*! a
        ip  = distribute h !*! inverse s !* y ^+^ distribute c !* iam
        iamp = distribute a !* ip
        c = ident !-!  k !*! h
        pp = p !-! (p !*! lamp !*! p)
        xp = x ^-^ p !*  ip

mvsmoother m@(LinearModel a  h g b vg vb vgb )  (x,p)  (xp,pp)  =  (xt,pt,pxt) 
    where 
        as = distribute (a !*! pp)  !*! inverse ( a !*! pp !*! distribute a  !+! processNoise m)
        s = pp !-! as !*! a !*! pp
        xt = as !* x ^+^ xp  ^-^ as !*! a !* xp
        pt = (1/2) *!! ((as !*! p  !*! distribute a !+! s ) !+! distribute ( as !*! p  !*! distribute a !+! s) )
        pxt = as !*! pt  !+! (xt  `outer` xp)

expectationMaximization vt (xt,pxt) pxtxt1  = (a,b ,sigA,sigB)
    where  a = an (xt,pxt) (tail xt,tail pxt) pxtxt1 
           b = bn vt (xt,pxt) 
           sigB = sigmaB b vt xt
           sigA = sigmaA a (xt,pxt) (tail xt,tail pxt) pxtxt1
           an (xt,pxt) (xt1,pxt1) (pxtxt1) = ((sumM $ zipWith outer xt  xt1)  !+!  sumM pxtxt1) !*!  inverse ((sumM $ zipWith outer  xt xt) !+! sumM pxt )
           bn  vt (xt,pxt)  = (sumM $ zipWith outer vt  xt) !*! inverse ((sumM $  zipWith  outer xt  xt) !+! (sumM pxt) )
           sigmaA a (xt,pxt) (xt1,pxt1) pxtxt1 = ((sumM $ zipWith outer  xt xt)  !+! (sumM pxt ) ) !-!  (a !*!  distribute ((sumM $ zipWith outer  xt  xt1) !+! (sumM pxtxt1) ))
           sigmaB b vt xt = (1/fromIntegral (length $ F.toList vt)) *!! ((sumM $ zipWith outer vt vt ) !-! b !*! distribute (sumM $  zipWith outer vt  xt) )

sumM :: (Additive t,Additive g,Num a ,F.Foldable f) => f (t ( g a)) -> t ( g a)
sumM = F.foldr1 (!+!) 
        

        
