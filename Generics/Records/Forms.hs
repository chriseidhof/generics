{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Generics.Records.Forms where

import Control.Applicative
import Control.Applicative.Error
import Control.Monad.Identity
import Generics.Records.Views
import Text.XHtml.Strict ((+++), (<<))
import qualified Text.XHtml.Strict as X
import qualified Text.XHtml.Strict.Formlets as F
import Text.Formlets (check)
import Generics.Records

data Form a = ToForm {toForm :: Maybe a -> F.XHtmlForm Identity a}

instance Applicative Identity where
  pure = return
  (<*>) = ap

instance Labeled Form where
  lconstant    = ToForm (\x -> check (F.input (show <$> x)) safeRead)
  -- lint         = ToForm (\x -> fromIntegral <$> F.inputInteger (fromIntegral <$> x))
  -- linteger     = F.inputInteger
  lprod  ra rb = ToForm $ \x -> (,) <$> toForm ra (fst <$> x) <*> (F.plug (\x -> X.br +++ x) $ toForm rb (snd <$> x))
  lfield s r   = ToForm $ \x -> F.plug (\h -> (X.label << (capitalize s ++ ": ") +++ h)) $ toForm r x
  ltype  ep ra = ToForm $ \a -> to ep <$> (toForm ra (from ep <$> a))

instance Rep Form String where
  rep = ToForm F.input

form :: (Rep Form a) => Maybe a -> F.XHtmlForm Identity a
form = toForm rep


-- Password type, should move to a separate "handy types" file.
instance Rep Form Password where
  rep = ToForm (\x -> Password <$> F.password (password <$> x))

safeRead s = maybeRead' s "Error parsing value"
