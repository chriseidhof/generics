{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Generics.Regular.ModelName where

import Generics.Regular

class GModelName f where
  gmodelName :: f a -> String

instance (Constructor c) => GModelName (C c f) where
  gmodelName x = conName x
