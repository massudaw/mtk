{-# LANGUAGE TemplateHaskell,TypeOperators#-}
module Product where

import Data.Monoid hiding(Product)
import Data.Functor.Product
import Data.Traversable
import Data.Functor.Compose
import Data.Distributive
import CartesianProduct
import Control.Applicative
import Control.Lens.TH
import SemiProduct
import Linear.Matrix
import Linear.Vector


class FProduct t where
  _ffst :: Functor f => (g a -> f ( g a)) -> t g h a -> f (t g h a)
  _fsnd :: Functor f => (h a -> f ( h a)) -> t g h a -> f (t g h a)


instance FProduct (:|:) where
  _ffst  f ( a :|: b ) = ( :|: b) <$> f a
  _fsnd f ( a :|: b ) = ( a :|: ) <$> f b

instance FProduct Product where
  _ffst  f ( a `Pair` b ) = ( `Pair` b) <$> f a
  _fsnd f ( a `Pair` b ) = ( a `Pair` ) <$> f b

instance FProduct (:>:)  where
  _ffst  f ( a :>: b ) = ( :>: b) <$> f a
  _fsnd f ( a :>: b ) = ( a :>: ) <$> f b


