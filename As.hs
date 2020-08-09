{-# LANGUAGE TemplateHaskell,TypeOperators,TypeSynonymInstances,MultiParamTypeClasses,FlexibleInstances,FlexibleContexts,FunctionalDependencies ,TypeSynonymInstances,RankNTypes,StandaloneDeriving#-}
module As where

import Control.Lens
import Data.Functor.Compose
import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.Combinators
import Control.Lens.Type

import Control.Lens.TH
import Control.Monad.Trans.State

import Linear.V1
import Linear.V2
import Linear.V3

import Data.Functor.Product
import Data.Functor

import Control.Lens.Internal
--import Space.Class

import Local
import Control.Applicative
import CartesianProduct

data Product3 f g h a
    = Product3 (f a) (g a) (h a)
    deriving(Show)




instance (PLength g ,PLength f)=> PLength (Product f g) where
  plength (Pair x y) = 1 + plength x

instance PLength V1 where
  plength _ = 1



class PLength  f where
  plength :: f a -> Int



type G g f a = g ( f a)
type M f a = f (f  a)

--alongIdentity :: Applicative f =>
--     (forall a . FLens g g f f a )
--     -> FLens (M g) (M g) (M f) (M f) a
--alongIdentity l = lens (getMatrix l ) (setMatrix l )

alongMatrix  :: Applicative f =>
     (forall a . FLens g g f f a )
     -> (forall l . Functor l => (M f b -> l (M f b)) -> M g b -> l (M g b) )
alongMatrix l = lens (getMatrix l ) (setMatrix l )

alongState ::(Functor g ,Applicative f)=>
     (forall a . FLens g g f f a )
     -> (forall l . Functor l => (M g b -> l (M f b)) -> M g b -> l (M g b) )
alongState l = lens id (setState l )

alongState' ::
     FLens g g f f a
     -> FLens g g g f a
alongState' l = lens id (\x y -> l .~ y $ x)



getState :: Functor g =>
     (forall a . FLens g g f f a )
     -> M g b
     -> g ( f b )
getState l x = fmap (^. l) $  x

setState :: Applicative f =>
     (forall a . FLens g g f f a )
     -> M g b
     -> M f b
     -> M g b
setState l k x =  l %~ (liftA2 (l .~) x ) $ k


getMatrix  :: Functor f =>
     (forall a . FLens g g f f a )
     -> M g b
     -> M f b
getMatrix l x = fmap (^. l) $  (x ^. l)

setMatrix  :: Applicative f =>
     (forall a . FLens g g f f a )
     -> M g b
     -> M f b
     -> M g b
setMatrix l k x =  l %~ (liftA2 (l .~) x ) $ k

--alongFBoth :: Applicative f => (forall a. FLens g g f f a ) -> FLens (g  :|: (M g) ) (g :|: (M g) ) (f :|: (M f) ) (f :|: (M f) ) a
--alongFBoth l = l |:| alongMatrix l



type FLens s t a b i = forall f. Functor f =>  (a i -> f (b i)) -> s i -> f (t i)

infixr 7 |:|
(|:|):: FLens g g f f i ->  FLens g g j j i -> FLens g g (f :|: j) (f :|: j) i
(|:|) f1 f2 = lens (alongGet' f1 f2) (alongSet' f1 f2)

(|.|):: FLens g g f f i ->  FLens g g j j i -> FLens g g (Product f j) (Product f j) i
(|.|) f1 f2 = lens (alongGet f1 f2) (alongSet f1 f2)

alongModify :: FLens g g f f i ->  FLens g g j j i -> (Product f j i -> Product f j  i) -> g i -> g i
alongModify fgg jgg pfj g =  case pfj ( Pair (g ^. fgg) (g ^. jgg) ) of
                          Pair x y -> fgg .~ x $ jgg .~ y $ g



alongSet :: FLens g g f f i ->  FLens g g j j i -> g i -> Product f j  i -> g i
alongSet fgg jgg g (Pair x y)  = fgg .~ x $ jgg .~ y $ g

alongSet' :: FLens g g f f i ->  FLens g g j j i -> g i ->(f:|:j) i -> g i
alongSet' fgg jgg g (x :|: y)  = fgg .~ x $ jgg .~ y $ g

alongGet :: FLens g g f f i ->  FLens g g j j i -> g i -> Product f j  i
alongGet fgg jgg g = Pair (g ^. fgg )(g ^. jgg)

alongGet' :: FLens g g f f i ->  FLens g g j j i -> g i ->(f:|:j) i
alongGet' fgg jgg g = (g ^. fgg ) :|: (g ^. jgg)
class Functor1 s t a b  | s -> a, t -> b, s b -> t, t a -> s where
    _f1 :: FLens s t a b i


instance Functor1 (Product a b) (Product a' b) a a'   where
    _f1 k ~(a `Pair` b) =(\a' -> (a' `Pair` b )) <$> k a

instance Functor1 (Product3 a b c) (Product3 a' b c) a a'   where
    _f1 k ~(Product3 a  b c) =(\a' -> (Product3 a' b c )) <$> k a

class Functor2 s t a b  | s -> a, t -> b, s b -> t, t a -> s where
    _f2 :: FLens s t a b i

instance Functor2 (Product a b) (Product a b') b b'   where
    _f2 k ~(a `Pair` b) =(\b' -> (a `Pair` b' )) <$> k b

instance Functor2 (Product3 a b c) (Product3 a b' c) b b'   where
    _f2 k ~(Product3 a  b c) =(\b' -> (Product3 a b' c ))<$> k b

example = Product3 (V1 1) (V2 2 3) (V1 1 )

