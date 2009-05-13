{-# LANGUAGE MultiParamTypeClasses #-}
module Generics.Records where


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
