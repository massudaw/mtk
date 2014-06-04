{-# LANGUAGE TypeOperators,FlexibleInstances,TypeSynonymInstances,ScopedTypeVariables,DeriveGeneric,NoMonomorphismRestriction,TemplateHaskell,TypeFamilies,DeriveFunctor,DeriveFoldable,DeriveTraversable #-}
module CartesianProduct where

import Data.Foldable
import Data.Functor.Compose
import Data.Traversable
import Control.Applicative
import Control.Monad
import Control.Arrow
import Data.Monoid
import Data.Distributive

import Foreign.FStorable
import Linear.Vector
import Local
import Control.Lens.TH
import GHC.Generics


data (f :|: g) a = (:|:) { _cfst :: ! (f a) , _csnd :: ! (g a)}deriving(Show,Read,Eq,Generic)

infixr 7 :|:

type CartesianProduct = (:|:)
makeLenses ''(:|:)


type instance Local (f :|: g) = (Local f) :|: (Local g )


instance (Applicative f,Applicative g,Additive f, Additive g) => Additive (f :|: g) where
    zero = zero :|: zero
    {-# INLINE zero #-}
    liftU2 f (a :|: b) (c :|: d) = liftU2 f a c :|: liftU2 f b d
    {-# INLINE liftU2 #-}
    liftI2 f (a :|: b) (c :|: d) = liftI2 f a c :|: liftI2 f b d
    {-# INLINE liftI2 #-}

instance (Functor f, Functor g) => Functor (f :|: g) where
    fmap f (x :|: y) = (fmap f x) :|: (fmap f y)

instance (Foldable f, Foldable g) => Foldable (f  :|: g) where
    foldMap f (x :|: y) = foldMap f x `mappend` foldMap f y

instance (Traversable f, Traversable g) => Traversable (f :|: g) where
    traverse f (x :|: y) = (:|:)<$> traverse f x <*> traverse f y

instance (Applicative f , Applicative g) => Applicative (CartesianProduct  f g ) where
    pure  x = pure x :|: pure x
    (f :|: g) <*> (i :|: k) = (f <*> i) :|: (g <*> k)

instance (Functor f, Functor g,Monad f , Monad g) => Monad (f  :|: g ) where
    return x  = return x :|: return x 
    a :|:  b >>= f =  a' :|: b' where
    	a' = join . fmap (_cfst . f) $ a
    	b' = join . fmap (_csnd . f) $ b

instance (Functor g , Monad g, Functor f, Distributive f , Distributive g,Monad f ) => Monad (Compose f g ) where
    return  = Compose . return .return 
    a >>= f = Compose a' where
	a' =  fmap join . join .  fmap distribute . unCompose . fmap (unCompose . f) $ a
        unCompose :: Compose f g a -> f (g a)
        unCompose (Compose x ) = x


instance (Distributive f,Distributive g) => Distributive (CartesianProduct f g) where
    distribute wp = (collect _cfst wp) :|: (collect _csnd wp) where


instance (FStorable i , FStorable j) => FStorable (i :|: j) where
    fsize  _ = fsize (undefined :: i a) + fsize (undefined :: j a)
    fpoke ptr (x :|: y) = fpoke ptr x >> fpokeByteOff ptr (fsize x) y
    fpeek ptr = liftM2 (:|:) (fpeek ptr) (fpeekByteOff ptr (fsize (undefined :: i a)))



