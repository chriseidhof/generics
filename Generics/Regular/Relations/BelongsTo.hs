{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Generics.Regular.Relations.BelongsTo where

import Control.Applicative
import Database.HDBC (toSql)
import Generics.Regular
import Generics.Regular.Database hiding (maybeRead, getOne)
import Generics.Regular.ModelName
import Generics.Regular.Database.Values
import Generics.Regular.Database.Parse
import Generics.Regular.Views
import Generics.Regular.Formlets
import qualified Text.XHtml.Strict as X
import qualified Generics.Regular.Database.Columns as C
import qualified Text.XHtml.Strict.Formlets as F

data BelongsTo a = BTNotFetched | BTId Int | BTFetched (Int, a)
 deriving (Show, Read)

instance ParseSql (BelongsTo a) where
  parsef = Just <$> ((maybe BTNotFetched BTId . maybeRead) <$> getOne) -- Can this be done easier?

instance C.Columns (BelongsTo a) where columns _ l  = [l ++ "_id"]
instance Values    (BelongsTo a) where values = toInt

instance Html (BelongsTo a) where
  html = const X.noHtml

instance Formlet (BelongsTo a) where
  formlet _ = pure BTNotFetched

-- A bit hacky
toInt BTNotFetched      = [toSql ""]
toInt (BTId i)          = [toSql i]
toInt (BTFetched (i,_)) = [toSql i]

-- Existential?
data RBelongsTo a b = RBT { btField  :: a -> BelongsTo b
                          , btUpdate :: a -> BelongsTo b -> a
                          }

fillBelongsTo :: ( Regular c, GParse (PF c), GColumns (PF c)
                 , GModelName (PF c), Show c
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
