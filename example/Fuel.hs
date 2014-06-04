{-# LANGUAGE TypeFamilies #-}
import Kalman 
import Space.Class
import Linear.Metric
import Linear.V1
import Local

-- Demonstrate the use of kalman fitler estimating 
-- fluid level of a tank

-- Model
-- Absolute Field
data Tank a 
  = Tank {  level :: a 
         } deriving (Show,Eq)


-- Tangent
data FuelTransaction a 
  = Consumption { rate :: a }
  deriving (Show , Eq)


type instance Local Tank = V1 

data MeasureParams a
  = Scale
  { min :: a
  , measuredlevel :: a
  , scale :: a 
  , max :: a  
  }deriving (Eq,Show) 


prediction :: V1 a -> Tank a -> Local Tank a   
prediction l _ = l 


instance Space Tank where
  (Tank l )  |+| V1 x = Tank  (l + x )
  (Tank l )  |-| (Tank lv) = V1 (l - lv)
