{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Generics.Regular.Database.Columns where

import Generics.Regular
import Generics.Regular.Extras

class Columns a where
  columns :: a -> String -> [String]

instance Columns Bool   where columns = const keep
instance Columns String where columns = const keep
instance Columns Int    where columns = const keep

class GColumns f where
  gtocolumnsf :: (a -> String -> [String]) -> f a -> String -> [String]

instance GColumns I where
  gtocolumnsf f = error "gtocolumnsf: No recursion allowed."

instance (Constructor c, GColumns f) => GColumns (C c f) where
  gtocolumnsf f x = gtocolumnsf f (unC x)

instance Columns a => GColumns (K a) where
 gtocolumnsf _ x = columns (unK x)

instance (GColumns f, GColumns g) => GColumns (f :*: g) where
  gtocolumnsf f x s = gtocolumnsf f (prodFst x) s ++ gtocolumnsf f (prodSnd x) s

instance (Selector s, GColumns f) => GColumns (S s f) where
  gtocolumnsf f x _ = gtocolumnsf f (unS x) (selName x)

-- | The a is a type witness, so it can safely be undefined or an error-value.
gtocolumns :: (Regular a, GColumns (PF a)) => a -> [String]
gtocolumns = flip rec ""
  where rec = gtocolumnsf rec . from

keep    l = [l]
ignore  _  = []

