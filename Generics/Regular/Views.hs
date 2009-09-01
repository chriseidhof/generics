{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Generics.Regular.Views where

import Data.Char (toUpper)
import Text.XHtml.Strict ((+++), (<<))
import qualified Text.XHtml.Strict as X

import Generics.Regular

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
  toText f s@(S x) = selName s ++ ": " ++ toText f x

gtoText :: (Regular a, GText (PF a)) => a -> String
gtoText x = toText gtoText (from x)

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
  ghtmlf f s@(S x) = X.label << ((capitalize $ selName s) ++ ": ") +++ ghtmlf f x

ghtml :: (Regular a, GHtml (PF a)) => a -> X.Html
ghtml x = ghtmlf ghtml (from x)


--   lconstant    = View (X.toHtml . show)
--   lunit        = View (const X.noHtml)
--   lprod ra rb  = View $ \(a,b) -> toView ra a +++ X.br +++ toView rb b
--   lfield l r   = View $ \v -> (X.label << (capitalize l ++ ": ")) +++ toView r v
--   lcon   l r   = View $ \v -> (X.h1 << capitalize l) +++ toView r v
--   ltype ep r   = View $ \v -> toView r (from ep v)
-- 
-- instance Rep View String where
--   rep = View X.toHtml

capitalize "" = ""
capitalize (c:cs) = toUpper c : cs
