{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Generics.Regular.Database.Values where

import Generics.Regular
import Database.HDBC

class Values a where
  values :: a -> [SqlValue]

instance Values String where values = return . toSql
instance Values Int    where values = return . toSql

class GValues f where
  gvaluesf :: (a -> [SqlValue]) -> f a -> [SqlValue]

instance GValues I where
  gvaluesf f x = f (unI x)

instance (Constructor c, GValues f) => GValues (C c f) where
  gvaluesf f x = gvaluesf f (unC x)

instance Values a => GValues (K a) where
  gvaluesf _ x = values (unK x)

instance (GValues f, GValues g) => GValues (f :*: g) where
  gvaluesf f (x :*: y) = gvaluesf f x ++ gvaluesf f y

instance (Selector s, GValues f) => GValues (S s f) where
  gvaluesf f x = gvaluesf f (unS x)

gvalues :: (Regular a, GValues (PF a)) => a -> [SqlValue]
gvalues = gvaluesf gvalues . from

gvalues' :: (Regular a, GValues (PF a)) => PF a a -> [SqlValue]
gvalues' = gvaluesf gvalues
