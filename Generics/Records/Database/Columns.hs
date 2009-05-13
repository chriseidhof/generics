{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Generics.Records.Database.Columns where

import Generics.Records

data Columns a = Columns {toColumns :: a -> String -> [String]}
instance Labeled Columns where
  lconstant    = keep
  lprod ra rb  = Columns $ \x     l -> toColumns ra undefined l ++ toColumns rb undefined l
  lfield l r   = Columns $ \x _ -> toColumns r x l
  lcon   l r   = Columns $ \x l -> toColumns r x l
  ltype ep r   = Columns $ \x l -> toColumns r (from ep x) l


instance Rep Columns String where
  rep = keep

keep   = Columns $ \_ l -> [l]
ignore = Columns $ \_ _ -> []

columns :: (Rep Columns a) => a -> [String]
columns x = toColumns rep x ""
