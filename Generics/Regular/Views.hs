{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Generics.Regular.Views where

import Text.XHtml.Strict ((+++), (<<))
import qualified Text.XHtml.Strict as X
import qualified Text.JSON as J

import Generics.Regular
import Generics.Regular.Extras

class GText f where
  toText :: (a -> String) -> f a -> String

instance GText I where
  toText f (I r) = f r

instance (Constructor c, GText f) => GText (C c f) where
  toText f cx@(C x) = toText f x

instance Show a => GText (K a) where
  toText _ (K x) = show x

instance (GText f, GText g) => GText (f :*: g) where
  toText f (x :*: y) = toText f x ++ "\n" ++ toText f y

instance (Selector s, GText f) => GText (S s f) where
  toText f s@(S x) = h s ++ ": " ++ toText f x

gtoText :: (Regular a, GText (PF a)) => a -> String
gtoText x = toText gtoText (from x)


-- Html stuff

class Html a where
  html :: a -> X.Html

instance Html Int    where html = X.toHtml . show
instance Html String where html = X.toHtml 

class GHtml f where
  ghtmlf :: (a -> X.Html) -> f a -> X.Html

instance GHtml I where
  ghtmlf f (I r) = f r

instance (Constructor c, GHtml f) => GHtml (C c f) where
  ghtmlf f cx@(C x) = (X.h1 << capitalize (conName cx)) +++ ghtmlf f x

instance Html a => GHtml (K a) where
  ghtmlf _ (K x) = html x

instance (GHtml f, GHtml g) => GHtml (f :*: g) where
  ghtmlf f (x :*: y) = ghtmlf f x +++ X.br +++ ghtmlf f y

instance (Selector s, GHtml f) => GHtml (S s f) where
  ghtmlf f s@(S x) = X.label << ((h s) ++ ": ") +++ ghtmlf f x

ghtml :: (Regular a, GHtml (PF a)) => a -> X.Html
ghtml x = ghtmlf ghtml (from x)


class Table a where
  table :: a -> X.Html

class GTable f where
  gtablef :: (a -> X.Html) -> f a -> X.Html

instance GTable I where
  gtablef f (I r) = f r

instance (Constructor c, GTable f) => GTable (C c f) where
  gtablef f cx@(C x) = X.tr << (gtablef f x)

instance Html a => GTable (K a) where
  gtablef _ (K x) = html x

instance (GTable f, GTable g) => GTable (f :*: g) where
  gtablef f (x :*: y) = gtablef f x +++ gtablef f y

instance (Selector s, GTable f) => GTable (S s f) where
  gtablef f s@(S x) = X.td << gtablef f x

gtableRow :: (Regular a, GTable (PF a)) => a -> X.Html
gtableRow x = gtablef gtableRow (from x)

gtable :: (Regular a, GTable (PF a)) => [a] -> X.Html
gtable xs = X.table << map gtableRow xs
