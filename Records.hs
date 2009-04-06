{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
module Records where

import Data.Char (toUpper)
import Text.XHtml.Strict ((+++), (<<))
import qualified Text.XHtml.Strict as X

class Rep g a where
  rep :: g a

data EP d r = EP {from :: d -> r, to :: r -> d}

class Labeled g where
  lconstant :: (Enum a, Eq a, Ord a, Read a, Show a) => g a
  lint      :: g Int
  lint      = lconstant
  linteger  :: g Integer
  linteger  = lconstant
  lfloat    :: g Float
  lfloat    = lconstant
  ldouble   :: g Double
  ldouble   = lconstant
  lchar     :: g Char
  lchar     = lconstant
  lunit     :: g ()
  lunit     = lconstant
  lprod     :: g a -> g b -> g (a, b)
  lfield    :: String -> g a -> g a
  lcon      :: String -> g a -> g a
  ltype     :: EP b a -> g a -> g b

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
