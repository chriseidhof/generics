{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Generics.Records.Database.Values where

import Generics.Records
import Database.HDBC

data Values a = Values {toValues :: a -> [SqlValue]}
instance Labeled Values where
  lconstant    = Values $ \x -> [toSql $ show x]
  lprod ra rb  = Values $ \(x,y) -> toValues ra x ++ toValues rb y
  lfield l r   = Values $ \x -> toValues r x
  lcon   l r   = Values $ \x -> toValues r x
  ltype ep r   = Values $ \x -> toValues r (from ep x)

instance Rep Values String where
  rep = Values $ \x -> [toSql x]

instance Rep Values Password where
  rep = Values $ \x -> [toSql $ password x]

ignore = Values $ \_ -> []

values :: (Rep Values a) => a -> [SqlValue]
values = toValues rep 
