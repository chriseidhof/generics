{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Generics.Records.Views where

import Data.Char (toUpper)
import Text.XHtml.Strict ((+++), (<<))
import qualified Text.XHtml.Strict as X

import Records

data ToText a = ToText {text :: a -> String}

instance Labeled ToText where
  lconstant    = ToText show
  lint         = lconstant
  linteger     = lconstant
  lfloat       = lconstant
  ldouble      = lconstant
  lchar        = lconstant
  lunit        = ToText (const "()")
  lprod  ra rb = ToText $ \(a, b) -> text ra a ++ "\n" ++ text rb b
  lfield s r   = ToText $ \x -> s ++ ": " ++ text r x
  lcon   s r   = r
  ltype  ep ra = ToText $ \a -> text ra (from ep a)

instance Rep ToText String where
  rep = ToText id

toText :: (Rep ToText a) => a -> String
toText = text rep

view :: (Rep View a) => a -> X.Html
view = toView rep

data View a = View {toView :: a -> X.Html}

instance Labeled View where
  lconstant    = View (X.toHtml . show)
  lunit        = View (const X.noHtml)
  lprod ra rb  = View $ \(a,b) -> toView ra a +++ X.br +++ toView rb b
  lfield l r   = View $ \v -> (X.label << (capitalize l ++ ": ")) +++ toView r v
  lcon   l r   = View $ \v -> (X.h1 << capitalize l) +++ toView r v
  ltype ep r   = View $ \v -> toView r (from ep v)

instance Rep View String where
  rep = View X.toHtml

capitalize "" = ""
capitalize (c:cs) = toUpper c : cs
