{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Relations.BelongsTo where

import Records
import Collect
import Database
import ModelName
import qualified Database.Columns as C
import Database.Values
import Database.Parse
import Control.Applicative

data BelongsTo a = BTNotFetched | BTId Int | BTFetched (Int, a)
 deriving Show

instance Rep Parse (BelongsTo a) where
  rep = Parse $ Just <$> ((maybe BTNotFetched BTId . maybeRead) <$> getOne) -- Can this be done easier?

instance Rep C.Columns (BelongsTo a) where rep = C.Columns $ \_ l -> [l ++ "_id"]
instance Rep Values    (BelongsTo a) where rep = ignore
instance Rep ModelName (BelongsTo a) where rep = err

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
