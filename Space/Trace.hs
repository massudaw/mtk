{-# LANGUAGE TypeOperators #-}
module Space.Trace where

import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4
import CartesianProduct
import Data.Functor.Compose
import Data.Functor.Product
import Data.Distributive

class Functor m => Trace m where
   trace :: m (m a) -> m a 

instance Trace V1 where
   trace (V1 x) = x 

instance Trace V2 where
   trace (V2 (V2 xx _) (V2 _ yy) ) = V2 xx yy

instance Trace V3 where
   trace (V3 (V3 xx _ _ ) (V3 _ yy _) (V3 _ _ zz )) = V3 xx yy zz

instance Trace V4 where
   trace (V4 (V4 xx _ _ _ ) (V4 _ yy _  _) (V4   _ _ zz _ ) (V4 _ _ _ tt) ) = V4 xx yy zz tt

instance (Trace f, Trace g ) => Trace (f `Product` g ) where
   trace ( xx  `Pair`  yy )  = trace (fmap pfst xx) `Pair` trace (fmap psnd yy)
	where pfst (Pair x y) = x 
	      psnd (Pair x y) = y

-- A cleaner cartesian product
-- instance (Trace f, Trace g ) => Trace (f :|:g ) where
--   trace ( xx  :|:  yy )  = trace (fmap _cfst xx) :|: trace (fmap _csnd yy)

instance (Distributive g,Trace g, Trace f) => Trace (Compose g f ) where
   trace  =  Compose . fmap trace . trace . fmap distribute .unCompose . fmap unCompose  
     where
        unCompose :: Compose f g a -> f (g a)
        unCompose (Compose x ) = x

