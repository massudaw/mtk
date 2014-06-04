module Data.FMonoid.Class where

import Data.Monoid


class FMonoid f  where
  fempty :: f a
  (^<>^) ::  f a -> f a -> f a


class FMonoid2 f where 
  fempty2 :: Monoid a => f a
  fappend2 :: Monoid a => f a -> f a -> f a
