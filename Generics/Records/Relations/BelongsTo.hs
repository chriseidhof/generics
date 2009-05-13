{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Generics.Records.Relations.BelongsTo where

import Control.Applicative
import Database.HDBC (toSql)
import Generics.Records
import Generics.Records.Database
import Generics.Records.ModelName
import Generics.Records.Database.Values
import Generics.Records.Database.Parse
import qualified Generics.Records.Database.Columns as C

data BelongsTo a = BTNotFetched | BTId Int | BTFetched (Int, a)
 deriving (Show, Read)

instance Rep Parse (BelongsTo a) where
  rep = Parse $ Just <$> ((maybe BTNotFetched BTId . maybeRead) <$> getOne) -- Can this be done easier?

instance Rep C.Columns (BelongsTo a) where rep = C.Columns $ \_ l  -> [l ++ "_id"]
instance Rep Values    (BelongsTo a) where rep = Values toInt
instance Rep ModelName (BelongsTo a) where rep = err

-- A bit hacky
toInt BTNotFetched      = [toSql ""]
toInt (BTId i)          = [toSql i]
toInt (BTFetched (i,_)) = [toSql i]

-- Existential?
data RBelongsTo a b = RBT { btField  :: a -> BelongsTo b
                          , btUpdate :: a -> BelongsTo b -> a
                          }

fillBelongsTo :: ( Rep Parse c, Rep C.Columns c
                 , Rep ModelName c, Show c
                 ) => m 
                   -> (RBelongsTo m c)
                   -> DB m
fillBelongsTo u bt = case btField bt u of
                       BTNotFetched    -> error "fillBelongsTo"
                       BTId x          -> do value <- find (fromBelongsTo $ btField bt u) x
                                             let value' = fromMaybe' "fillBelongsTo" value
                                             return $ btUpdate bt u $ BTFetched (x, value')
                       x@(BTFetched _) -> return $ btUpdate bt u x
 where fromBelongsTo (BTFetched (x,y)) = y
       fromBelongsTo _                 = error "fromBelongsTo"

fromMaybe' e (Just x) = x
fromMaybe' e _        = error e
