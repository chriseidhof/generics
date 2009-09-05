{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Generics.Regular.Formlets where

import Control.Applicative
import Control.Applicative.Error
import Control.Monad.Identity
import Text.XHtml.Strict ((+++), (<<))
import qualified Text.XHtml.Strict as X
import qualified Text.XHtml.Strict.Formlets as F
import Text.Formlets (check)
import Generics.Regular
import Generics.Regular.Extras

type XFormlet m a = F.XHtmlFormlet m a

class    Formlet a      where formlet :: (Functor m, Applicative m, Monad m) => XFormlet m a
instance Formlet Int    where formlet x = fromIntegral <$> F.inputInteger (toInteger <$> x)
instance Formlet String where formlet = F.input


class GFormlet f where
  gformf :: (Functor m, Applicative m, Monad m) => XFormlet m a -> XFormlet m (f a)

gformlet :: (Regular a, GFormlet (PF a), Functor m, Applicative m, Monad m) => XFormlet m a
gformlet x = to <$> (gformf gformlet (from <$> x))

instance (Constructor c, GFormlet f) => GFormlet (C c f) where
  gformf f x = C <$> (gformf f $ unC <$> x)

instance Formlet a => GFormlet (K a) where
  gformf _ x = K <$> (formlet (unK <$> x))

instance (GFormlet f, GFormlet g) => GFormlet (f :*: g) where
  gformf f x = (:*:) <$> (gformf f (prodFst <$> x)) <* F.xml X.br <*> (gformf f (prodSnd <$> x))

 
instance (Selector s, GFormlet f) => GFormlet (S s f) where
  gformf f x = F.plug ((X.label << (h (fromJust x) ++ ": ")) +++) $ S <$> gformf f (unS <$> x)
   where fromJust (Just x) = x
