{-# LANGUAGE
 StandaloneDeriving,
 FlexibleContexts,
 MultiParamTypeClasses,
 TypeOperators,
 FlexibleInstances,
 DeriveFunctor,
 GeneralizedNewtypeDeriving,
 DeriveDataTypeable,
 TypeFamilies
 #-}

module Rotation.SH2 where

import Data.Data

import Control.Lens

import Linear.V2
import Linear.V1

import Local

type instance Local SO2 = V1

type M2 a = V2 (V2 a)

data SO2 a = SO2 {unSO2 :: M2 a}deriving(Functor,Data,Typeable,Eq,Show,Read)


rotationSO2 (V1 t)
    = V2
        (V2 (cos t) (sin t))
        (V2 (-sin t) (cos t))

skewM2 :: Num a => V1 a -> M2 a
skewM2 (V1 xy)
    = V2
        (V2 0 xy)
        (V2 (-xy) 0)


