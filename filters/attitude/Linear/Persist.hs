{-# LANGUAGE FlexibleInstances #-}
module Linear.Persist where

import Linear.V3
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import qualified Data.Text as T



instance  PersistFieldSql (V3 Double) where 
    sqlType _ = SqlString
instance  PersistField (V3 Double) where 
    toPersistValue  (V3 x y z) = PersistList [toPersistValue x, toPersistValue y, toPersistValue z]
    fromPersistValue (PersistList (vx:vy:vz:[])) =
      case (fromPersistValue vx, fromPersistValue vy,fromPersistValue vz) of
        (Right x,Right y,Right z) -> Right (V3 x y z)
        (Left e, _,_) -> Left e
        (_, Left e,_) -> Left e
        (_, _,Left e) -> Left e
    fromPersistValue x = Left $ T.pack $ "Expected 2 item PersistList, received: " ++ show x

