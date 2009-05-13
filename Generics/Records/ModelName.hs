{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Generics.Records.ModelName where

import Generics.Records

data ModelName a = ModelName {toModelName :: a -> String}
instance Labeled ModelName where
  lconstant    = err
  lprod ra rb  = err
  lfield l r   = err
  lcon   l r   = ModelName $ \_ -> l
  ltype ep r   = ModelName $ \x -> toModelName r (from ep x)


instance Rep ModelName String where
  rep = err

instance Rep ModelName Password where
  rep = err

err = error "ModelName not defined"

modelName :: (Rep ModelName a) => a -> String
modelName x = toModelName rep x 
